#' Function to split data in different ways for conditioning
#'
#' Utility function to split data frames up in various ways for conditioning
#' plots. Widely used by many `openair` functions usually through the option
#' `type`.
#'
#' This section give a brief description of each of the define levels of `type`.
#' Note that all time dependent types require a column `date`.
#'
#' - `"default"` does not split the data but will describe the levels as a date
#' range in the format "day month year".
#'
#' - `"year"` splits the data by each year.
#'
#' - `"month"` splits the data by month of the year.
#'
#' - `"hour"` splits the data by hour of the day.
#'
#' - `"monthyear"` splits the data by year and month. It differs from month in
#' that a level is defined for each month of the data set. This is useful
#' sometimes to show an ordered sequence of months if the data set starts half
#' way through a year; rather than starting in January.
#'
#' - `"weekend"` splits the data by weekday and weekend.
#'
#' - `"weekday"` splits the data by day of the week - ordered to start Monday.
#'
#' - `"season"` splits data up by season. In the northern hemisphere winter =
#' December, January, February; spring = March, April, May etc. These
#' definitions will change of `hemisphere = "southern"`.
#'
#' - `"seasonyear"` (or `"yearseason"`) will split the data into year-season
#' intervals, keeping the months of a season together. For example, December
#' 2010 is considered as part of winter 2011 (with January and February 2011).
#' This makes it easier to consider contiguous seasons. In contrast, `type =
#' "season"` will just split the data into four seasons regardless of the year.
#'
#' - `"daylight"` splits the data relative to estimated sunrise and sunset to
#' give either daylight or nighttime. The cut is made by `cutDaylight` but more
#' conveniently accessed via `cutData`, e.g. `cutData(mydata, type = "daylight",
#' latitude = my.latitude, longitude = my.longitude)`. The daylight estimation,
#' which is valid for dates between 1901 and 2099, is made using the measurement
#' location, date, time and astronomical algorithms to estimate the relative
#' positions of the Sun and the measurement location on the Earth's surface, and
#' is based on NOAA methods. Measurement location should be set using `latitude`
#' (+ to North; - to South) and `longitude` (+ to East; - to West).
#'
#' - `"dst"` will split the data by hours that are in daylight saving time (DST)
#' and hours that are not for appropriate time zones. The option also requires
#' that the local time zone is given e.g. `local.tz = "Europe/London"`,
#' `local.tz = "America/New_York"`. Each of the two periods will be in
#' *local time*. The main purpose of this option is to test whether there
#' is a shift in the diurnal profile when DST and non-DST hours are compared.
#' This option is particularly useful with the [timeVariation()] function. For
#' example, close to the source of road vehicle emissions, "rush-hour" will tend
#' to occur at the same *local time* throughout the year, e.g., 8 am and 5 pm.
#' Therefore, comparing non-DST hours with DST hours will tend to show similar
#' diurnal patterns (at least in the timing of the peaks, if not magnitude) when
#' expressed in local time. By contrast a variable such as wind speed or
#' temperature should show a clear shift when expressed in local time. In
#' essence, this option when used with `timeVariation()` may help determine
#' whether the variation in a pollutant is driven by man-made emissions or
#' natural processes.
#'
#' - `"wd"` splits the data by 8 wind sectors and requires a column `wd`: "NE",
#' "E", "SE", "S", "SW", "W", "NW", "N".
#'
#' Note that all the date-based types, e.g., `"month"`/`"year"` are derived from
#' a column `date`. If a user already has a column with a name of one of the
#' date-based types it will not be used.
#'
#' @param x A data frame containing a field `date`.
#' @param type A string giving the way in which the data frame should be split.
#'   Pre-defined values are: `"default"`, `"year"`, `"hour"`, `"month"`,
#'   `"season"`, `"weekday"`, `"site"`, `"weekend"`, `"monthyear"`,
#'   `"daylight"`, `"dst"` (daylight saving time).
#'
#'   `type` can also be the name of a numeric or factor. If a numeric column
#'   name is supplied [cutData()] will split the data into four quantiles.
#'   Factors levels will be used to split the data without any adjustment.
#' @param names By default, the columns created by [cutData()] are named after
#'   their `type` option. Specifying `names` defines other names for the columns,
#'   which map onto the `type` options in the same order they are given. The
#'   length of `names` should therefore be equal to the length of `type`.
#' @param suffix If `name` is not specified, `suffix` will be appended to any
#'   added columns that would otherwise overwrite existing columns. For example,
#'   `cutData(mydata, "nox", suffix = "_cuts")` would append a `nox_cuts` column
#'   rather than overwriting `nox`.
#' @param hemisphere Can be `"northern"` or `"southern"`, used to split data
#'   into seasons.
#' @param n.levels Number of quantiles to split numeric data into.
#' @param start.day What day of the week should the `type = "weekday"` start on?
#'   The user can change the start day by supplying an integer between 0 and 6.
#'   Sunday = 0, Monday = 1, ... For example to start the weekday plots on a
#'   Saturday, choose `start.day = 6`.
#' @param is.axis A logical (`TRUE`/`FALSE`), used to request shortened cut
#'   labels for axes.
#' @param local.tz Used for identifying whether a date has daylight savings time
#'   (DST) applied or not. Examples include `local.tz = "Europe/London"`,
#'   `local.tz = "America/New_York"`, i.e., time zones that assume DST.
#'   <https://en.wikipedia.org/wiki/List_of_zoneinfo_time_zones> shows time
#'   zones that should be valid for most systems. It is important that the
#'   original data are in GMT (UTC) or a fixed offset from GMT.
#' @param latitude,longitude The decimal latitude and longitudes used when `type
#'   = "daylight"`. Note that locations west of Greenwich have negative
#'   longitudes.
#' @param ... All additional parameters are passed on to next function(s).
#' @export
#' @return Returns the data frame, `x`, with columns appended as defined by
#'   `type` and `name`.
#' @author David Carslaw
#' @author Karl Ropkins (`"daylight"` option)
#' @examples
#' ## split data by day of the week
#' mydata <- cutData(mydata, type = "weekday")
#' names(mydata)
#' head(mydata)
cutData <- function(x,
                    type = "default",
                    names = NULL,
                    suffix = NULL,
                    hemisphere = "northern",
                    n.levels = 4,
                    start.day = 1,
                    is.axis = FALSE,
                    local.tz = NULL,
                    latitude = 51,
                    longitude = -0.5,
                    ...) {
  if (!is.null(names)) {
    if (length(names) != length(type)) {
      cli::cli_abort("Length of {.field names} ({.val {length(names)}}) not equal to length of {.field type} ({.val {length(type)}}).")
    }
  }
  
  makeCond <- function(x, name = NULL, type = "default") {
    if (is.null(names)) {
      name <- type
      if (!is.null(suffix)) {
        while (name %in% names(x)) {
          name <- paste0(name, suffix)
        }
      }
    }
    
    # reserved types
    conds <- c(
      "default",
      "year",
      "hour",
      "month",
      "season",
      "week",
      "weekday",
      "wd",
      "weekend",
      "monthyear",
      "yearmonth",
      "bstgmt",
      "gmtbst",
      "dst",
      "daylight",
      "seasonyear",
      "yearseason"
    )

    # if conditioning type already built in, is present in data frame and is a
    # factor
    if (type %in% conds && type %in% names(x)) {
      if (is.factor(x[[type]])) {
        x[[name]] <- factor(x[[type]]) ## remove unused factor levels
        return(x)
      }
    }

    # if not using an in-built condition, assume it's a column in the dataframe and handle appropriately
    if (!type %in% conds) {
      # error if 'type' isn't in the document
      if (!type %in% names(x)) {
        cli::cli_abort(call = NULL,
          c(
            "x" = "{.field type} '{type}' is neither a built-in option, nor a column in {.field x}.",
            "i" = "{.emph Built-ins:} {conds}",
            "i" = "{.emph Names in {.field x}}: {names(x)}"
          )
        )
      }

      # drop missing values in the dataframe, if any exist
      x <- dropNAbyType(x, type)

      # split by quantiles if numeric, else set to factor
      if (inherits(x[[type]], c("numeric", "integer"))) {
        x[[name]] <- cutVecNumeric(x[[type]],
          type = type,
          n.levels = n.levels,
          is.axis = is.axis
        )
      } else {
        x[[name]] <- factor(x[[type]])
      }

      return(x)
    }

    if (type == "default") {
      # shows dates (if available)
      # not always available e.g. scatterPlot
      if ("date" %in% names(x)) {
        x[[name]] <- factor(paste(
          format(min(x$date), "%d %B %Y"),
          " to ",
          format(max(x$date), "%d %B %Y"),
          sep = ""
        ))
        ## order the data by date
        x <- arrange(x, date)
      } else {
        x[[name]] <- factor("all data")
      }
    }

    if (type == "year") {
      x[[name]] <- cutVecYear(x$date)
    }

    if (type == "hour") {
      x[[name]] <- cutVecHour(x$date)
    }

    if (type == "month") {
      x[[name]] <- cutVecMonth(x$date, is.axis = is.axis)
    }

    if (type %in% c("monthyear", "yearmonth")) {
      x[[name]] <- cutVecMonthyear(x$date, is.axis = is.axis)
    }

    if (type == "week") {
      x[[name]] <- cutVecWeek(x$date)
    }

    if (type == "season") {
      x[[name]] <- cutVecSeason(x$date, hemisphere = hemisphere)
    }

    if (type %in% c("seasonyear", "yearseason")) {
      x[[name]] <- cutVecSeasonyear(x$date, hemisphere = hemisphere)
    }

    if (type == "weekend") {
      x[[name]] <- cutVecWeekend(x$date)
    }

    if (type == "weekday") {
      x[[name]] <- cutVecWeekday(x$date, start.day = start.day)
    }

    if (type == "wd") {
      x <- dropNAbyType(x, "wd")
      x[[name]] <- cutVecWinddir(x$wd)
    }

    if (type %in% c("dst", "bstgmt", "gmtbst")) {
      type <- "dst" ## keep it simple
      x[[name]] <- cutVecDST(x$date, local.tz = local.tz)
    }

    if (type == "daylight") {
      x[[name]] <- cutVecDaylight(x$date, latitude, longitude, ...)
    }

    return(x)
  }

  for (i in seq_along(type)) {
    x <- makeCond(x, name = names[i], type = type[i])
  }
  return(x)
}

#' Drop missing values and warn that it has happened
#' @noRd
dropNAbyType <- function(x, type) {
  if (anyNA(x[[type]])) {
    lenNA <- length(which(is.na(x[[type]])))
    x <- x[!is.na(x[[type]]), ]
    cli::cli_warn(c(
      "!" = "Removing {.val {lenNA}} rows due to missing {.col {type}} data."
    ))
  }
  return(x)
}

#' Cut a numeric vector into quantiles
#' @noRd
cutVecNumeric <- function(x, type, n.levels, is.axis) {
  temp.levels <-
    levels(cut(x, unique(quantile(
      x,
      probs = seq(0, 1,
        length =
          n.levels + 1
      ),
      na.rm = TRUE
    )), include.lowest = TRUE))

  x <- cut(x,
    unique(quantile(
      x,
      probs = seq(0, 1, length = n.levels + 1),
      na.rm = TRUE
    )),
    include.lowest = TRUE,
    labels = FALSE
  )

  x <- as.factor(x)
  temp.levels <- gsub("[(]|[)]|[[]|[]]", "", temp.levels)
  temp.levels <- gsub("[,]", " to ", temp.levels)
  levels(x) <- if (is.axis) {
    temp.levels
  } else {
    paste(type, temp.levels)
  }
  return(x)
}

#' Cut a vector into a 'year' factor
#' @noRd
cutVecYear <- function(x) {
  ordered(lubridate::year(x))
}

#' Cut a vector into a 'hour' factor
#' @noRd
cutVecHour <- function(x) {
  ordered(lubridate::hour(x))
}

#' Cut a vector into a factor of weeks of the year
#' @noRd
cutVecWeek <- function(x) {
  x <- format(x, "%W")
  x <- ordered(x, levels = unique(x))
  return(x)
}

#' Cut a date vector into weekday/weekend
#' @noRd
cutVecWeekend <- function(x) {
  wdays <- lubridate::wday(x, week_start = 1L)
  x <- dplyr::case_match(wdays, 1:5 ~ "weekday", 6:7 ~ "weekend")
  x <- ordered(x, levels = c("weekday", "weekend"))
  return(x)
}

#' Cut a date vector into weekdays
#' @noRd
cutVecWeekday <- function(x, start.day) {
  if (start.day == 0L) {
    start.day <- 7L
  }
  x <- lubridate::wday(x, label = TRUE, abbr = FALSE, week_start = start.day)
  levels <- levels(x)
  levels <- levels[levels %in% x]
  x <- ordered(x, levels = levels)
  return(x)
}

#' Cut a vector into an ordered 'month' factor with no empty levels
#' @noRd
cutVecMonth <- function(x, is.axis) {
  x <- lubridate::month(x, label = TRUE, abbr = is.axis)
  levs <- levels(x)[levels(x) %in% x]
  x <- factor(x, levels = levs, ordered = TRUE)
  return(x)
}

#' Cut a vector into 'monthyears' (e.g., January 2020)
#' @noRd
cutVecMonthyear <- function(x, is.axis) {
  str <- "%B %Y"
  if (is.axis) {
    str <- "%b %Y"
  }

  x <- format(x, str)
  x <- ordered(x, levels = unique(x))
  return(x)
}

#' Cut into season year (e.g., Summer 2020)
#' @noRd
cutVecSeasonyear <- function(x, hemisphere) {
  seasons <- cutVecSeason(x, hemisphere = hemisphere)
  years <- lubridate::year(x)
  months <- lubridate::month(x)
  years[months == 12] <- years[months == 12] + 1L

  x <- paste(seasons, "-", years)
  x <- ordered(x, levels = unique(x))
  return(x)
}

#' Cut wind direction into bins
#' @noRd
cutVecWinddir <- function(x) {
  x <- cut(
    x,
    breaks = seq(22.5, 382.5, 45),
    labels = c("NE", "E", "SE", "S", "SW", "W", "NW", "N")
  )

  x[is.na(x)] <- "N" # for wd < 22.5

  x <- ordered(x, levels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"))

  return(x)
}

#' Cut dates into DST
#' @noRd
cutVecDST <- function(x, local.tz) {
  ## how to extract BST/GMT
  if (is.null(local.tz)) {
    message("missing time zone, assuming Europe/London")
    local.tz <- "Europe/London"
  }

  attr(x, "tzone") <- local.tz

  isdst <- as.POSIXlt(x)$isdst

  if (any(isdst == -1)) {
    cli::cli_abort("Not possible to identify DST for {.field local.tz} '{local.tz}'.")
  }

  x <- dplyr::case_match(isdst, 0 ~ "Non-DST", 1 ~ "DST")

  x <- factor(x, levels = sort(unique(x)))

  return(x)
}

#' Cut date vector into daylight or not
#'
#' calculations use (lat, long) position relative to sun to estimate if daylight
#' or nighttime hour solar.noon.lst, etc are factions of day seconds into that
#' day = p.time * 86400 so for example sunset time is as.POSIXct(sunset.time.lst
#' * 86400, origin = format(x$date, "%Y-%m-%d")) (assuming you do not run into
#' next day!) currently unsure about extremes long nights and days at poles need
#' checking
#'
#' @noRd
cutVecDaylight <- function(x,
                           latitude = 51.522393,
                           longitude = -0.154700,
                           ...) {
  # back-compatibility
  x <- data.frame(date = x)

  # local hour offset

  local.hour.offset <- as.numeric(lubridate::force_tz(x$date[1], "UTC") - x$date[1])

  ###################
  # temp functions
  ###################
  rad <- function(x) {
    x * pi / 180
  }
  degrees <- function(x) {
    x * (180 / pi)
  }

  ###############
  # get local time
  ###############
  temp <- x$date

  #################
  # make julian.refs
  #################
  # ref Gregorian calendar back extrapolated.
  # assumed good for years between 1800 and 2100

  p.day <- (as.numeric(format(temp, "%H")) * 3600) +
    (as.numeric(format(temp, "%M")) * 60) +
    as.numeric(format(temp, "%S"))
  p.day <- p.day / 86400

  # julian century (via julian day)
  julian.century <-
    as.numeric(as.Date(temp, format = "%m/%d/%Y")) + 2440587.5 +
    p.day - (local.hour.offset / 24)
  julian.century <- (julian.century - 2451545) / 36525

  ##################
  # main calcs
  ##################
  # as of noaa

  geom.mean.long.sun.deg <-
    (280.46646 + julian.century * (36000.76983 + julian.century * 0.0003032)) %% 360

  geom.mean.anom.sun.deg <-
    357.52911 + julian.century * (35999.05029 - 0.0001537 * julian.century)

  eccent.earth.orbit <-
    0.016708634 - julian.century * (0.000042037 + 0.0001537 * julian.century)

  sun.eq.of.ctr <- sin(rad(geom.mean.anom.sun.deg)) *
    (1.914602 - julian.century * (0.004817 + 0.000014 * julian.century)) +
    sin(rad(2 * geom.mean.anom.sun.deg)) *
      (0.019993 - 0.000101 * julian.century) +
    sin(rad(3 * geom.mean.anom.sun.deg)) * 0.000289

  sun.true.long.deg <- sun.eq.of.ctr + geom.mean.long.sun.deg

  sun.app.long.deg <- sun.true.long.deg - 0.00569 - 0.00478 *
    sin(rad(125.04 - 1934.136 * julian.century))

  mean.obliq.ecliptic.deg <- 23 + (26 + ((
    21.448 - julian.century *
      (46.815 + julian.century *
        (0.00059 - julian.century
        * 0.001813))
  )) / 60) / 60

  obliq.corr.deg <- mean.obliq.ecliptic.deg +
    0.00256 * cos(rad(125.04 - 1934.136 * julian.century))

  sun.declin.deg <- degrees(asin(sin(rad(obliq.corr.deg)) *
    sin(rad(sun.app.long.deg))))

  vary <- tan(rad(obliq.corr.deg / 2)) * tan(rad(obliq.corr.deg / 2))

  eq.of.time.minutes <-
    4 * degrees(
      vary * sin(2 * rad(geom.mean.long.sun.deg)) -
        2 * eccent.earth.orbit * sin(rad(geom.mean.anom.sun.deg)) +
        4 * eccent.earth.orbit * vary * sin(rad(geom.mean.anom.sun.deg)) *
          cos(2 * rad(geom.mean.long.sun.deg)) - 0.5 * vary * vary *
          sin(4 * rad(geom.mean.long.sun.deg)) - 1.25 * eccent.earth.orbit *
          eccent.earth.orbit * sin(2 * rad(geom.mean.anom.sun.deg))
    )

  # original nooa code
  ##
  # ha.sunrise.deg <- degrees(acos(cos(rad(90.833)) /
  #                  (cos(rad(latitude)) * cos(rad(sun.declin.deg))) -
  #                  tan(rad(latitude)) * tan(rad(sun.declin.deg))))
  ##
  # R error catcher added
  # for long nights>24hours/short nights<0

  ha.sunrise.deg <- cos(rad(90.833)) /
    (cos(rad(latitude)) * cos(rad(sun.declin.deg))) -
    tan(rad(latitude)) * tan(rad(sun.declin.deg))
  ha.sunrise.deg <- ifelse(ha.sunrise.deg > 1, 1, ha.sunrise.deg)
  ha.sunrise.deg <- ifelse(ha.sunrise.deg < -1, -1, ha.sunrise.deg)
  ha.sunrise.deg <- degrees(acos(ha.sunrise.deg))

  solar.noon.lst <-
    (720 - 4 * longitude - eq.of.time.minutes + local.hour.offset * 60) / 1440

  sunrise.time.lst <- solar.noon.lst - ha.sunrise.deg * 4 / 1440

  sunset.time.lst <- solar.noon.lst + ha.sunrise.deg * 4 / 1440

  sunlight.duration.minutes <- 8 * ha.sunrise.deg

  #################################
  # daylight factor
  #################################
  # need to confirm dusk/dawn handing

  daylight <- ifelse(
    sunlight.duration.minutes == 0,
    FALSE,
    ifelse(
      sunlight.duration.minutes == 1440,
      TRUE,
      ifelse(
        sunrise.time.lst < sunset.time.lst,
        ifelse(p.day < sunset.time.lst &
          p.day > sunrise.time.lst, TRUE, FALSE),
        ifelse(
          p.day <= sunrise.time.lst &
            p.day >= sunset.time.lst,
          FALSE,
          TRUE
        )
      )
    )
  )
  # as ordered factor
  daylight <-
    factor(
      daylight,
      levels = c(TRUE, FALSE),
      labels = c("daylight", "nighttime")
    )

  return(daylight)
}

#' Cut a vector into seasons
#' @noRd
cutVecSeason <- function(x, hemisphere) {
  hemisphere <- rlang::arg_match(hemisphere, c("northern", "southern"))
  
  # need to work out month names local to the user and extract first letter
  month_names_local <-
    cutVecMonth(ISOdate(2000, 1:12, 1), is.axis = FALSE) %>%
    substr(1, 1)
  
  # Function to create, e.g., 'winter (JFM)'
  make_season_name <- function(str, id) {
    paste0(str, " (", paste(month_names_local[id], collapse = ""), ")")
  }
  
  # get months by number
  month_ids <- lubridate::month(x)
  
  # split
  if (hemisphere == "northern") {
    x <-
      dplyr::case_match(
        month_ids,
        c(12, 1, 2) ~ make_season_name("winter", c(12, 1, 2)),
        c(3, 4, 5) ~ make_season_name("spring", c(3, 4, 5)),
        c(6, 7, 8) ~ make_season_name("summer", c(6, 7, 8)),
        c(9, 10, 11) ~ make_season_name("autumn", c(9, 10, 11))
      )
    
    seasons <-
      c(
        make_season_name("spring", c(3, 4, 5)),
        make_season_name("summer", c(6, 7, 8)),
        make_season_name("autumn", c(9, 10, 11)),
        make_season_name("winter", c(12, 1, 2))
      )
    
    seasons <- seasons[seasons %in% x]
    
    x <- ordered(x, levels = seasons)
  } else {
    x <-
      dplyr::case_match(
        month_ids,
        c(12, 1, 2) ~ make_season_name("summer", c(12, 1, 2)),
        c(3, 4, 5) ~ make_season_name("autumn", c(3, 4, 5)),
        c(6, 7, 8) ~ make_season_name("winter", c(6, 7, 8)),
        c(9, 10, 11) ~ make_season_name("spring", c(9, 10, 11))
      )
    
    seasons <-
      c(
        make_season_name("spring", c(9, 10, 11)),
        make_season_name("summer", c(12, 1, 2)),
        make_season_name("autumn", c(3, 4, 5)),
        make_season_name("winter", c(6, 7, 8))
      )
    
    seasons <- seasons[seasons %in% x]
    
    x <- ordered(x, levels = seasons)
  }
  
  return(x)
}
