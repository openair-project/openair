#' Function to split data in different ways for conditioning
#'
#' Utility function to split data frames up in various ways for conditioning
#' plots. Widely used by many `openair` functions usually through the option
#' `type`.
#'
#' This section give a brief description of each of the define levels of `type`.
#' Note that all time dependent types require a column `date`.
#'
#'  - `"default"` does not split the data but will describe the levels as a date
#' range in the format "day month year".
#'
#'  - `"year"` splits the data by each year.
#'
#'  - `"month"` splits the data by month of the year.
#'
#'  - `"hour"` splits the data by hour of the day.
#'
#'  - `"monthyear"` splits the data by year and month. It differs from month in
#' that a level is defined for each month of the data set. This is useful
#' sometimes to show an ordered sequence of months if the data set starts half
#' way through a year; rather than starting in January.
#'
#'  - `"weekend"` splits the data by weekday and weekend.
#'
#'  - `"weekday"` splits the data by day of the week - ordered to start Monday.
#'
#'  - `"season"` splits data up by season. In the northern hemisphere winter =
#' December, January, February; spring = March, April, May etc. These
#' definitions will change of `hemisphere = "southern"`.
#'
#'  - `"seasonyear"` (or `"yearseason"`) will split the data into year-season
#' intervals, keeping the months of a season together. For example, December
#' 2010 is considered as part of winter 2011 (with January and February 2011).
#' This makes it easier to consider contiguous seasons. In contrast, `type =
#' "season"` will just split the data into four seasons regardless of the year.
#'
#'  - `"daylight"` splits the data relative to estimated sunrise and sunset to
#' give either daylight or nighttime. The cut is made by `cutDaylight` but more
#' conveniently accessed via `cutData`, e.g. `cutData(mydata, type = "daylight",
#' latitude = my.latitude, longitude = my.longitude)`. The daylight estimation,
#' which is valid for dates between 1901 and 2099, is made using the measurement
#' location, date, time and astronomical algorithms to estimate the relative
#' positions of the Sun and the measurement location on the Earth's surface, and
#' is based on NOAA methods. Measurement location should be set using `latitude`
#' (+ to North; - to South) and `longitude` (+ to East; - to West).
#'
#'  - `"dst"` will split the data by hours that are in daylight saving time (DST)
#' and hours that are not for appropriate time zones. The option also requires
#' that the local time zone is given e.g. `local.tz = "Europe/London"`,
#' `local.tz = "America/New_York"`. Each of the two periods will be in
#'  *local time*. The main purpose of this option is to test whether there
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
#'  - `"wd"` splits the data by 8 wind sectors and requires a column `wd`: "NE",
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
#'   their `type` option. Specifying `names` defines other names for the
#'   columns, which map onto the `type` options in the same order they are
#'   given. The length of `names` should therefore be equal to the length of
#'   `type`.
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
#' @param start.season What order should the season be. By default, the order is spring, summer, autumn, winter. `start.season = "winter"` would plot winter first.
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
#' @param drop How to handle empty factor levels. One of:
#'
#'  - `"default"`: Sensible defaults selected on a case-by-case basis for
#'   different `type` options.
#'
#'  - `"empty"`: Drop all empty factor levels.
#'
#'  - `"none"`: Retain all empty factor levels, where possible. For example,
#'   for `type = "hour"`, all factor levels from `0` and `23` will be
#'   represented.
#'
#'  - `"outside"`: Retain empty factor levels within the range of the data.
#'   For example, for `type = "hour"` when the data only contains data for 1 AM
#'   and 5 AM, the factor levels, `1`, `2`, `3`, `4` and `5` will be retained.
#'
#'   Some of these options only apply to certain `type` options. For example,
#'   for `type = "year"`, `"outside"` is equivalent to `"none"` as there is no
#'   fixed range of years to use in the `"none"` case.
#' @param ... All additional parameters are passed on to next function(s).
#' @export
#' @return Returns the data frame, `x`, with columns appended as defined by
#'   `type` and `name`.
#' @author David Carslaw
#' @author Jack Davison
#' @author Karl Ropkins (`"daylight"` option)
#' @examples
#' # split data by day of the week
#' mydata <- cutData(mydata, type = "weekday")
#' names(mydata)
#' head(mydata)
cutData <- function(
  x,
  type = "default",
  names = NULL,
  suffix = NULL,
  hemisphere = "northern",
  n.levels = 4,
  start.day = 1,
  start.season = "spring",
  is.axis = FALSE,
  local.tz = NULL,
  latitude = 51,
  longitude = -0.5,
  drop = c("default", "empty", "outside", "none"),
  ...
) {
  drop <- rlang::arg_match(drop, c("default", "empty", "outside", "none"))

  if (!is.null(names)) {
    if (length(names) != length(type)) {
      cli::cli_abort(
        "Length of {.field names} ({.val {length(names)}}) not equal to length of {.field type} ({.val {length(type)}})."
      )
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
        cli::cli_abort(
          call = NULL,
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
        x[[name]] <- cutVecNumeric(
          x[[type]],
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
      x[[name]] <- cutVecYear(x$date, drop = drop)
    }

    if (type == "hour") {
      x[[name]] <- cutVecHour(x$date, drop = drop)
    }

    if (type == "month") {
      x[[name]] <- cutVecMonth(x$date, is.axis = is.axis, drop = drop)
    }

    if (type %in% c("monthyear", "yearmonth")) {
      x[[name]] <- cutVecMonthyear(x$date, is.axis = is.axis, drop = drop)
    }

    if (type == "week") {
      x[[name]] <- cutVecWeek(x$date, drop = drop)
    }

    if (type == "season") {
      x[[name]] <- cutVecSeason(
        x$date,
        hemisphere = hemisphere,
        is.axis = is.axis,
        drop = drop,
        start.season = start.season
      )
    }

    if (type %in% c("seasonyear", "yearseason")) {
      x[[name]] <- cutVecSeasonyear(
        x$date,
        hemisphere = hemisphere,
        is.axis = is.axis,
        drop = drop
      )
    }

    if (type == "weekend") {
      x[[name]] <- cutVecWeekend(x$date, drop = drop)
    }

    if (type == "weekday") {
      x[[name]] <- cutVecWeekday(
        x$date,
        is.axis = is.axis,
        start.day = start.day,
        drop = drop
      )
    }

    if (type == "wd") {
      x <- dropNAbyType(x, "wd")
      x[[name]] <- cutVecWinddir(x$wd, drop = drop)
    }

    if (type %in% c("dst", "bstgmt", "gmtbst")) {
      type <- "dst" ## keep it simple
      x[[name]] <- cutVecDST(x$date, local.tz = local.tz, drop = drop)
    }

    if (type == "daylight") {
      x[[name]] <- cutVecDaylight(x$date, latitude, longitude, ..., drop = drop)
    }

    return(x)
  }

  for (i in seq_along(type)) {
    x <- makeCond(x, name = names[i], type = type[i])
  }
  return(x)
}

# Drop missing values and warn that it has happened
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

# Cut a numeric vector into quantiles
cutVecNumeric <- function(x, type, n.levels, is.axis) {
  temp.levels <-
    levels(cut(
      x,
      unique(quantile(
        x,
        probs = seq(0, 1, length = n.levels + 1),
        na.rm = TRUE
      )),
      include.lowest = TRUE
    ))

  x <- cut(
    x,
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

# Cut a vector into a 'year' factor
cutVecYear <- function(x, drop) {
  x <- lubridate::year(x)
  if (drop %in% c("default", "empty")) {
    x <- ordered(x)
  } else {
    x_range <- range(unique(x), na.rm = TRUE)
    levels <- seq(x_range[1], x_range[2], by = 1L)
    x <- ordered(x, levels = levels)
  }
  x
}

# Cut a vector into a 'hour' factor
cutVecHour <- function(x, drop) {
  x <- lubridate::hour(x)
  if (drop %in% c("none")) {
    x <- ordered(x, levels = 0:23)
  } else if (drop == "outside") {
    x_range <- range(unique(x), na.rm = TRUE)
    levels <- seq(x_range[1], x_range[2], by = 1L)
    x <- ordered(x, levels = levels)
  } else if (drop %in% c("default", "empty")) {
    x <- ordered(x)
  }
  x
}

# Cut a vector into a factor of weeks of the year
cutVecWeek <- function(x, drop) {
  x <- as.numeric(format(x, "%W"))
  if (drop %in% c("none")) {
    x <- ordered(strpad(x, 2), levels = strpad(0:53))
  } else if (drop == "outside") {
    x_range <- range(unique(x), na.rm = TRUE)
    levels <- seq(x_range[1], x_range[2], by = 1L)
    x <- ordered(strpad(x, 2), levels = strpad(levels, 2))
  } else if (drop %in% c("default", "empty")) {
    x <- ordered(strpad(x, 2), levels = strpad(unique(x), 2))
  }
  return(x)
}

# Cut a date vector into weekday/weekend
cutVecWeekend <- function(x, drop) {
  wdays <- lubridate::wday(x, week_start = 1L)
  x <- dplyr::case_match(wdays, 1:5 ~ "weekday", 6:7 ~ "weekend")
  unique_vals <- unique(x)
  if (drop %in% c("default", "none")) {
    levels <- c("weekday", "weekend")
  } else {
    levels <- unique_vals
  }
  x <- ordered(x, levels = levels)
  return(x)
}

# Cut a date vector into weekdays
cutVecWeekday <- function(x, is.axis, start.day, drop) {
  if (start.day == 0L) {
    start.day <- 7L
  }
  x <- lubridate::wday(x, label = TRUE, abbr = is.axis, week_start = start.day)
  levels <- levels(x)
  if (drop %in% c("default", "empty")) {
    levels <- levels[levels %in% x]
  } else if (drop == "none") {
    levels <- levels
  } else if (drop == "outside") {
    levels
    x_range <- range(unique(as.numeric(x)))
    levels_int <- seq(x_range[1], x_range[2], by = 1L)
    levels <- levels[levels_int]
  }
  x <- ordered(x, levels = levels)
  return(x)
}

# Cut a vector into an ordered 'month' factor
cutVecMonth <- function(x, is.axis, drop) {
  x <- lubridate::month(x, label = TRUE, abbr = is.axis)
  levels <- levels(x)
  if (drop %in% c("default", "empty")) {
    levels <- levels[levels %in% x]
  } else if (drop == "none") {
    levels <- levels
  } else if (drop == "outside") {
    x_range <- range(unique(as.numeric(x)))
    levels_int <- seq(x_range[1], x_range[2], by = 1L)
    levels <- levels[levels_int]
  }
  x <- ordered(x, levels = levels)
  return(x)
}

# Cut a vector into 'monthyears' (e.g., January 2020)
cutVecMonthyear <- function(x, is.axis, drop) {
  str <- "%B %Y"
  if (is.axis) {
    str <- "%b\n%Y"
  }

  # get years and months
  yrs <- cutVecYear(x, drop = "none")
  mnths <- cutVecMonth(x, is.axis = is.axis, drop = "none")

  # get combinations of years and months
  levels <-
    tidyr::crossing(
      year = factor(levels(yrs), levels = levels(yrs)),
      month = factor(levels(mnths), levels = levels(mnths))
    ) |>
    dplyr::mutate(
      level = paste(.data$month, .data$year, sep = " ")
    ) |>
    dplyr::pull(.data$level)

  # combine actual years and months
  x <- paste(mnths, yrs, sep = " ")

  # get the factor levels
  if (drop %in% c("default", "empty")) {
    levels <- unique(x)
  } else if (drop == "none") {
    levels <- levels
  } else if (drop == "outside") {
    start <- which(levels == dplyr::first(x))
    end <- which(levels == dplyr::last(x))
    levels <- levels[start:end]
  }

  x <- ordered(x, levels = levels)

  return(x)
}

# Cut into season year (e.g., Summer 2020)
cutVecSeasonyear <- function(x, hemisphere, is.axis, drop) {
  sep <- ifelse(is.axis, "\n", "-")

  # get seasons/years
  seasons <- cutVecSeason(
    x,
    hemisphere = hemisphere,
    is.axis = TRUE,
    drop = "none"
  )
  years <- lubridate::year(x)

  # adjust year if month is 12 - belongs to "winter" next year
  months <- lubridate::month(x)
  years[months == 12] <- years[months == 12] + 1L

  # get combinations of years and months
  levels <-
    tidyr::crossing(
      year = seq(min(years, na.rm = TRUE), max(years, na.rm = TRUE), by = 1L),
      season = factor(levels(seasons), levels = levels(seasons))
    ) |>
    dplyr::mutate(
      level = paste(.data$season, .data$year, sep = sep)
    ) |>
    dplyr::pull(.data$level)

  # combine actual years and months
  x <- factor(paste(seasons, years, sep = sep), levels = levels)

  # get the factor levels
  if (drop %in% c("default", "empty")) {
    levels <- unique(x)
  } else if (drop == "none") {
    levels <- levels
  } else if (drop == "outside") {
    level_ids <- range(which(levels %in% x))
    levels <- levels[min(level_ids):max(level_ids)]
  }

  x <- ordered(x, levels = levels)

  return(x)
}

# Cut wind direction into bins
cutVecWinddir <- function(x, drop) {
  x <- cut(
    x,
    breaks = seq(22.5, 382.5, 45),
    labels = c("NE", "E", "SE", "S", "SW", "W", "NW", "N")
  )

  x[is.na(x)] <- "N" # for wd < 22.5

  levels <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")

  if (drop %in% c("default", "none", "outside")) {
    levels <- levels
  } else if (drop == "empty") {
    levels <- levels[levels %in% x]
  }

  x <- ordered(x, levels = levels)

  return(x)
}

# Cut dates into DST
cutVecDST <- function(x, local.tz, drop) {
  ## how to extract BST/GMT
  if (is.null(local.tz)) {
    cli::cli_warn("missing time zone, assuming Europe/London")
    local.tz <- "Europe/London"
  }

  attr(x, "tzone") <- local.tz

  isdst <- as.POSIXlt(x)$isdst

  if (any(isdst == -1)) {
    cli::cli_abort(
      "Not possible to identify DST for {.field local.tz} '{local.tz}'."
    )
  }

  x <- dplyr::case_match(isdst, 0 ~ "Non-DST", 1 ~ "DST")

  if (drop %in% c("default", "empty", "outside")) {
    x <- factor(x, levels = unique(x))
  } else if (drop == "none") {
    x <- factor(x, levels = c("DST", "Non-DST"))
  }

  return(x)
}

# Cut date vector into daylight or not
#
# calculations use (lat, long) position relative to sun to estimate if daylight
# or nighttime hour solar.noon.lst, etc are factions of day seconds into that
# day = p.time * 86400 so for example sunset time is as.POSIXct(sunset.time.lst
# * 86400, origin = format(x$date, "%Y-%m-%d")) (assuming you do not run into
# next day!) currently unsure about extremes long nights and days at poles need
# checking
cutVecDaylight <- function(
  x,
  latitude = 51.522393,
  longitude = -0.154700,
  ...,
  drop
) {
  # back-compatibility
  x <- data.frame(date = x)

  # local hour offset

  local.hour.offset <- as.numeric(
    lubridate::force_tz(x$date[1], "UTC") - x$date[1]
  )

  # temp functions
  rad <- function(x) {
    x * pi / 180
  }
  degrees <- function(x) {
    x * (180 / pi)
  }

  # get local time
  temp <- x$date

  # make julian.refs
  # ref Gregorian calendar back extrapolated.
  # assumed good for years between 1800 and 2100

  p.day <- (as.numeric(format(temp, "%H")) * 3600) +
    (as.numeric(format(temp, "%M")) * 60) +
    as.numeric(format(temp, "%S"))
  p.day <- p.day / 86400

  # julian century (via julian day)
  julian.century <-
    as.numeric(as.Date(temp, format = "%m/%d/%Y")) +
    2440587.5 +
    p.day -
    (local.hour.offset / 24)
  julian.century <- (julian.century - 2451545) / 36525

  # main calcs
  # as of noaa

  geom.mean.long.sun.deg <-
    (280.46646 + julian.century * (36000.76983 + julian.century * 0.0003032)) %%
    360

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

  sun.app.long.deg <- sun.true.long.deg -
    0.00569 -
    0.00478 *
      sin(rad(125.04 - 1934.136 * julian.century))

  mean.obliq.ecliptic.deg <- 23 +
    (26 +
      ((21.448 -
        julian.century *
          (46.815 +
            julian.century *
              (0.00059 - julian.century * 0.001813)))) /
        60) /
      60

  obliq.corr.deg <- mean.obliq.ecliptic.deg +
    0.00256 * cos(rad(125.04 - 1934.136 * julian.century))

  sun.declin.deg <- degrees(asin(
    sin(rad(obliq.corr.deg)) *
      sin(rad(sun.app.long.deg))
  ))

  vary <- tan(rad(obliq.corr.deg / 2)) * tan(rad(obliq.corr.deg / 2))

  eq.of.time.minutes <-
    4 *
    degrees(
      vary *
        sin(2 * rad(geom.mean.long.sun.deg)) -
        2 * eccent.earth.orbit * sin(rad(geom.mean.anom.sun.deg)) +
        4 *
          eccent.earth.orbit *
          vary *
          sin(rad(geom.mean.anom.sun.deg)) *
          cos(2 * rad(geom.mean.long.sun.deg)) -
        0.5 * vary * vary * sin(4 * rad(geom.mean.long.sun.deg)) -
        1.25 *
          eccent.earth.orbit *
          eccent.earth.orbit *
          sin(2 * rad(geom.mean.anom.sun.deg))
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

  # daylight factor
  # need to confirm dusk/dawn handing

  daylight <- ifelse(
    sunlight.duration.minutes == 0,
    FALSE,
    ifelse(
      sunlight.duration.minutes == 1440,
      TRUE,
      ifelse(
        sunrise.time.lst < sunset.time.lst,
        ifelse(
          p.day < sunset.time.lst &
            p.day > sunrise.time.lst,
          TRUE,
          FALSE
        ),
        ifelse(
          p.day <= sunrise.time.lst &
            p.day >= sunset.time.lst,
          FALSE,
          TRUE
        )
      )
    )
  )

  x <- dplyr::if_else(
    daylight,
    "daylight",
    false = "nighttime",
    missing = NA
  )

  if (drop %in% c("default", "empty", "outside")) {
    x <- factor(x, levels = unique(x))
  } else if (drop == "none") {
    x <- factor(x, levels = c("daylight", "nighttime"))
  }

  return(x)
}

# Cut a vector into seasons
cutVecSeason <- function(
  x,
  hemisphere,
  is.axis,
  drop,
  start.season = "spring"
) {
  sep <- ifelse(is.axis, "\n", " ")
  hemisphere <- rlang::arg_match(hemisphere, c("northern", "southern"))

  # Validate start.season
  valid_seasons <- c("spring", "summer", "autumn", "winter")
  start.season <- rlang::arg_match(start.season, valid_seasons)

  # need to work out month names local to the user and extract first letter
  month_names_local <-
    cutVecMonth(ISOdate(2000, 1:12, 1), is.axis = FALSE, drop = "none") |>
    substr(1, 1)

  # Function to create, e.g., 'winter (JFM)'
  make_season_name <- function(str, id) {
    paste0(str, sep, "(", paste(month_names_local[id], collapse = ""), ")")
  }

  # 1. Generate a named list of all season labels for the current hemisphere
  # This ensures the text in the data matches the levels exactly.
  if (hemisphere == "northern") {
    season_map <- c(
      spring = make_season_name("spring", c(3, 4, 5)),
      summer = make_season_name("summer", c(6, 7, 8)),
      autumn = make_season_name("autumn", c(9, 10, 11)),
      winter = make_season_name("winter", c(12, 1, 2))
    )
  } else {
    season_map <- c(
      spring = make_season_name("spring", c(9, 10, 11)),
      summer = make_season_name("summer", c(12, 1, 2)),
      autumn = make_season_name("autumn", c(3, 4, 5)),
      winter = make_season_name("winter", c(6, 7, 8))
    )
  }

  # 2. Assign the season label to the data vector (x)
  month_ids <- lubridate::month(x)

  if (hemisphere == "northern") {
    x_str <- dplyr::case_match(
      month_ids,
      c(12, 1, 2) ~ season_map[["winter"]],
      c(3, 4, 5) ~ season_map[["spring"]],
      c(6, 7, 8) ~ season_map[["summer"]],
      c(9, 10, 11) ~ season_map[["autumn"]]
    )
  } else {
    x_str <- dplyr::case_match(
      month_ids,
      c(12, 1, 2) ~ season_map[["summer"]],
      c(3, 4, 5) ~ season_map[["autumn"]],
      c(6, 7, 8) ~ season_map[["winter"]],
      c(9, 10, 11) ~ season_map[["spring"]]
    )
  }

  # 3. Determine the order of levels based on start.season
  # Find index of start.season in the standard list
  start_index <- match(start.season, valid_seasons)

  # Rotate the season keys (e.g. if start is winter (4): 4, 1, 2, 3)
  rotated_indices <- c(
    start_index:4,
    seq_len(start_index - 1)
  )

  ordered_keys <- valid_seasons[rotated_indices]

  # Extract the formatted text levels in the correct order
  levels <- unname(season_map[ordered_keys])

  # 4. Handle drop logic
  if (drop %in% c("default", "empty")) {
    levels <- levels[levels %in% x_str]
  } else if (drop %in% c("none", "outside")) {
    levels <- levels
  }

  # 5. Return ordered factor
  ordered(x_str, levels = levels)
}
