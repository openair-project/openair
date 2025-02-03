#' Function to split data in different ways for conditioning
#'
#' Utility function to split data frames up in various ways for conditioning
#' plots. Users would generally not be expected to call this function directly.
#' Widely used by many \code{openair} functions usually through the option
#' \code{type}.
#'
#' This section give a brief description of each of the define levels of
#' \code{type}. Note that all time dependent types require a column \code{date}.
#'
#' "default" does not split the data but will describe the levels as a date
#' range in the format "day month year".
#'
#' "year" splits the data by each year.
#'
#' "month" splits the data by month of the year.
#'
#' "hour" splits the data by hour of the day.
#'
#' "monthyear" splits the data by year and month. It differs from month in that
#' a level is defined for each month of the data set. This is useful sometimes
#' to show an ordered sequence of months if the data set starts half way through
#' a year; rather than starting in January.
#'
#' "weekend" splits the data by weekday and weekend.
#'
#' "weekday" splits the data by day of the week - ordered to start Monday.
#'
#' "season" splits data up by season. In the northern hemisphere winter =
#' December, January, February; spring = March, April, May etc. These
#' definitions will change of \code{hemisphere = "southern"}.
#'
#' "seasonyear (or "yearseason") will split the data into year-season intervals,
#' keeping the months of a season together. For example, December 2010 is
#' considered as part of winter 2011 (with January and February 2011). This
#' makes it easier to consider contiguous seasons. In contrast, \code{type =
#' "season"} will just split the data into four seasons regardless of the year.
#'
#' "daylight" splits the data relative to estimated sunrise and sunset to give
#' either daylight or nighttime. The cut is made by \code{cutDaylight} but more
#' conveniently accessed via \code{cutData}, e.g. \code{cutData(mydata, type =
#' "daylight", latitude = my.latitude, longitude = my.longitude)}. The daylight
#' estimation, which is valid for dates between 1901 and 2099, is made using the
#' measurement location, date, time and astronomical algorithms to estimate the
#' relative positions of the Sun and the measurement location on the Earth's
#' surface, and is based on NOAA methods. Measurement location should be set
#' using \code{latitude} (+ to North; - to South) and \code{longitude} (+ to
#' East; - to West).
#'
#' "dst" will split the data by hours that are in daylight saving time (DST) and
#' hours that are not for appropriate time zones. The option "dst" also requires
#' that the local time zone is given e.g. \code{local.tz = "Europe/London"},
#' \code{local.tz = "America/New_York"}. Each of the two periods will be in
#' \emph{local time}. The main purpose of this option is to test whether there
#' is a shift in the diurnal profile when DST and non-DST hours are compared.
#' This option is particularly useful with the \code{timeVariation} function.
#' For example, close to the source of road vehicle emissions, `rush-hour' will
#' tend to occur at the same \emph{local time} throughout the year e.g. 8 am and
#' 5 pm. Therefore, comparing non-DST hours with DST hours will tend to show
#' similar diurnal patterns (at least in the timing of the peaks, if not
#' magnitude) when expressed in local time. By contrast a variable such as wind
#' speed or temperature should show a clear shift when expressed in local time.
#' In essence, this option when used with \code{timeVariation} may help
#' determine whether the variation in a pollutant is driven by man-made
#' emissions or natural processes.
#'
#' "wd" splits the data by 8 wind sectors and requires a column \code{wd}: "NE",
#' "E", "SE", "S", "SW", "W", "NW", "N".
#'
#' "ws" splits the data by 8 quantiles of wind speed and requires a column
#' \code{ws}.
#'
#' "site" splits the data by site and therefore requires a column \code{site}.
#'
#' Note that all the date-based types e.g. month/year are derived from a column
#' \code{date}. If a user already has a column with a name of one of the
#' date-based types it will not be used.
#'
#' @param x A data frame containing a field \code{date}.
#' @param type A string giving the way in which the data frame should be split.
#'   Pre-defined values are: \dQuote{default}, \dQuote{year}, \dQuote{hour},
#'   \dQuote{month}, \dQuote{season}, \dQuote{weekday}, \dQuote{site},
#'   \dQuote{weekend}, \dQuote{monthyear}, \dQuote{daylight}, \dQuote{dst}
#'   (daylight saving time).
#'
#'   \code{type} can also be the name of a numeric or factor. If a numeric
#'   column name is supplied \code{cutData} will split the data into four
#'   quantiles. Factors levels will be used to split the data without any
#'   adjustment.
#' @param hemisphere Can be \code{"northern"} or \code{"southern"}, used to
#'   split data into seasons.
#' @param n.levels Number of quantiles to split numeric data into.
#' @param start.day What day of the week should the \code{type = "weekday"}
#'   start on?  The user can change the start day by supplying an integer
#'   between 0 and 6. Sunday = 0, Monday = 1, \ldots For example to start the
#'   weekday plots on a Saturday, choose \code{start.day = 6}.
#' @param is.axis A logical (\code{TRUE}/\code{FALSE}), used to request
#'   shortened cut labels for axes.
#' @param local.tz Used for identifying whether a date has daylight savings time
#'   (DST) applied or not. Examples include \code{local.tz = "Europe/London"},
#'   \code{local.tz = "America/New_York"} i.e. time zones that assume DST.
#'   \url{https://en.wikipedia.org/wiki/List_of_zoneinfo_time_zones} shows time
#'   zones that should be valid for most systems. It is important that the
#'   original data are in GMT (UTC) or a fixed offset from GMT. See
#'   \code{import} and the openair manual for information on how to import data
#'   and ensure no DST is applied.
#' @param latitude The decimal latitude used in \code{type = "daylight"}.
#' @param longitude The decimal longitude. Note that locations west of Greenwich
#'   are negative.
#' @param ... All additional parameters are passed on to next function(s).
#' @export
#' @return Returns a data frame with a column \code{cond} that is defined by
#'   \code{type}.
#' @author David Carslaw (cutData) and Karl Ropkins (cutDaylight)
#' @examples
#'
#' ## split data by day of the week
#' mydata <- cutData(mydata, type = "weekday")
#'
#'
cutData <- function(x, type = "default", hemisphere = "northern",
                    n.levels = 4, start.day = 1, is.axis = FALSE,
                    local.tz = NULL, latitude = 51, longitude = -0.5,
                    ...) {

  ## function to cutData depending on choice of variable
  ## pre-defined types and user-defined types
  ## If another added, then amend checkPrep

  ## note: is.axis modifies factor levels to give shorter labels for axis
  ##       generic label shortening handled at end of section
  ##       format(date, "%?") outputs modified by is.axis are set using temp
  ##       declared at at start of associated type section - karl

  # so we know that openair has looked at the data
  attr(x, "source") <- "openair"


  makeCond <- function(x, type = "default") {

    ## if type is time based and already exists in data,
    ## just return data

    # if (type %in% dateTypes & type %in% names(x) & attr(x, "source") != "openair") {
    #   message(paste0("\nUsing ", "'", type, "'", " in data frame and not date-based openair version. \nThis may result in different behaviour compared with openair calculations."))
    #   return(x)
    # }

    conds <- c(
      "default", "year", "hour", "month", "season", "week",
      "weekday", "wd", "site", "weekend", "monthyear",
      "bstgmt", "gmtbst", "dst", "daylight", "seasonyear",
      "yearseason"
    )

    ## if conditioning type already built in, is present in data frame and is a factor
    if (type %in% conds & type %in% names(x)) {
      if (is.factor(x[[type]])) {
        x[[type]] <- factor(x[[type]]) ## remove unused factor levels
        return(x)
      }
    }

    if (type %in% conds == FALSE) { ## generic, user-defined
      ## split by quantiles unless it is a factor, in which case keep as is
      ## number of quantiles set by n.levels

      # don't want missing values in cuts
      if (anyNA(x[[type]])) {
        lenNA <- length(which(is.na(x[[type]])))

        x <- x[!is.na(x[[type]]), ]

        warning(paste0("removing ", lenNA, " missing rows due to ", type), call. = FALSE)
      }

      if (is.factor(x[[type]]) | is.character(x[[type]]) | class(x[[type]])[1] == "Date" |
        "POSIXt" %in% class(x[[type]])) {

        ## drop unused levels while we are at it
        x[[type]] <- factor(x[[type]])
      } else {
        temp.levels <-
          levels(cut(
            x[[type]], unique(quantile(
              x[[type]],
              probs = seq(
                0, 1,
                length =
                  n.levels + 1
              ),
              na.rm = TRUE
            )),
            include.lowest = TRUE
          ))

        x[[type]] <- cut(
          x[[type]],
          unique(quantile(
            x[[type]],
            probs = seq(0, 1, length = n.levels + 1),
            na.rm = TRUE
          )),
          include.lowest = TRUE,
          labels = FALSE
        )

        x[[type]] <- as.factor(x[[type]])
        temp.levels <- gsub("[(]|[)]|[[]|[]]", "", temp.levels)
        temp.levels <- gsub("[,]", " to ", temp.levels)
        levels(x[[type]]) <- if (is.axis) temp.levels else paste(type, temp.levels)
      }
    }

    if (type == "default") {
      ## shows dates (if available)
      ## not always available e.g. scatterPlot
      if ("date" %in% names(x)) {
        x[[type]] <- factor(paste(
          format(min(x$date), "%d %B %Y"), " to ",
          format(max(x$date), "%d %B %Y"),
          sep = ""
        ))
        ## order the data by date
        x <- arrange(x, date)
      } else {
        x[[type]] <- factor("all data")
      }
    }

    if (type == "year") x[[type]] <- factor(lubridate::year(x$date))

    if (type == "hour") x[[type]] <- factor(format(x$date, "%H"))

    if (type == "month") {
      ## need to generate month abbrevs on the fly for different languages
      temp <- if (is.axis) "%b" else "%B"
      x[[type]] <- format(x$date, temp)

      ## month names
      month.abbs <- format(seq(
        as.Date("2000-01-01"),
        as.Date("2000-12-31"), "month"
      ), temp)

      ## might only be partial year...
      ids <- which(month.abbs %in% unique(x$month))
      the.months <- month.abbs[ids]

      x[[type]] <- ordered(x[[type]], levels = the.months)
    }

    if (type == "monthyear") {
      x[[type]] <- format(x$date, "%B %Y")
      x[[type]] <- ordered(x[[type]], levels = unique(x[[type]]))
    }

    if (type == "week") {
      x[[type]] <- format(x$date, "%W")
      x[[type]] <- ordered(x[[type]], levels = unique(x[[type]]))
    }

    if (type == "season") {
      ## need to generate month abbrevs on the fly for different languages
      temp <- if (is.axis) "%b" else "%B"
      x[[type]] <- format(x$date, temp)

      ## month names
      month.abbs <- format(seq(
        as.Date("2000-01-01"),
        as.Date("2000-12-31"), "month"
      ), temp)

      make_month_abbr <- function(x){
        month.starts <- substr(month.abbs, 1, 1)
        paste(month.starts[x], collapse = "")
      }

      if (!hemisphere %in% c("northern", "southern")) {
        stop("hemisphere must be 'northern' or 'southern'")
      }

      if (hemisphere == "northern") {
        ## define all as winter first, then assign others
        x[[type]] <- paste0("winter (", make_month_abbr(c(12,1,2)), ")")
        ids <- which(as.numeric(format(x$date, "%m")) %in% 3:5)
        x[[type]][ids] <- paste0("spring (", make_month_abbr(3:5), ")")
        ids <- which(as.numeric(format(x$date, "%m")) %in% 6:8)
        x[[type]][ids] <- paste0("summer (", make_month_abbr(6:8), ")")
        ids <- which(as.numeric(format(x$date, "%m")) %in% 9:11)
        x[[type]][ids] <- paste0("autumn (", make_month_abbr(9:11), ")")

        seasons <- c(
          paste0("spring (", make_month_abbr(3:5), ")"),
          paste0("summer (", make_month_abbr(6:8), ")"),
          paste0("autumn (", make_month_abbr(9:11), ")"),
          paste0("winter (", make_month_abbr(c(12, 1:2)), ")")
        )

        ## might only be partial year...
        ids <- which(seasons %in% unique(x$season))
        the.season <- seasons[ids]
        x[[type]] <- ordered(x[[type]], levels = the.season)
      }
      if (hemisphere == "southern") {
        ## define all as winter first, then assign others
        x[[type]] <- paste0("summer (", make_month_abbr(c(12,1,2)), ")")
        ids <- which(as.numeric(format(x$date, "%m")) %in% 3:5)
        x[[type]][ids] <- paste0("autumn (", make_month_abbr(c(3:5)), ")")
        ids <- which(as.numeric(format(x$date, "%m")) %in% 6:8)
        x[[type]][ids] <- paste0("winter (", make_month_abbr(c(6:8)), ")")
        ids <- which(as.numeric(format(x$date, "%m")) %in% 9:11)
        x[[type]][ids] <- paste0("spring (", make_month_abbr(c(9:11)), ")")

        seasons <- c(
          paste0("spring (", make_month_abbr(c(9:11)), ")"),
          paste0("summer (", make_month_abbr(c(12,1,2)), ")"),
          paste0("autumn (", make_month_abbr(c(3:5)), ")"),
          paste0("winter (", make_month_abbr(c(6:8)), ")")
        )

        ## might only be partial year...
        ids <- which(seasons %in% unique(x$season))
        the.season <- seasons[ids]
        x[[type]] <- ordered(
          x[[type]],
          levels = seasons
        )
      }
    }

    if (type %in% c("seasonyear", "yearseason")) {

      # this cuts data to ensure that a season spans two years to keep it together
      # For example, winter 2015 is considered  Dec. 2014 and Jan. Feb, 2015

      x <- cutData(x, type = "season", hemisphere = hemisphere)
      ## remove any missing seasons e.g. through type = "season"
      x <- x[!is.na(x$season), ]

      ## calculate year
      x <- mutate(
        x,
        year = lubridate::year(date),
        month = lubridate::month(date)
      )

      ## ids where month = 12, make December part of following year's season
      ids <- which(x$month == 12)
      x$year[ids] <- x$year[ids] + 1

      labels <- paste(x$season, "-", x$year)
      x[[type]] <- labels
      x[[type]] <- ordered(x[[type]], levels = unique(x[[type]]))
    }

    if (type == "weekend") {
      ## split by weekend/weekday
      weekday <- selectByDate(x, day = "weekday")
      weekday[[type]] <- "weekday"
      weekend <- selectByDate(x, day = "weekend")
      weekend[[type]] <- "weekend"

      x <- rbind(weekday, weekend)
      x[[type]] <- ordered(x[[type]], levels = c("weekday", "weekend"))
    }

    if (type == "weekday") {
      x[[type]] <- format(x$date, "%A")
      # weekday.names <-  format(ISOdate(2000, 1, 3:9), "%A")
      weekday.names <- format(ISOdate(2000, 1, 2:8), "%A")

      if (start.day < 0 || start.day > 6) {
        stop("start.day must be between 0 and 6.")
      }

      day.ord <- c(
        weekday.names[(1 + start.day):7],
        weekday.names[1:(1 + start.day - 1)]
      )

      ## might only be certain days available...
      ids <- which(weekday.names %in% unique(x$weekday))
      # the.days <- weekday.names[ids]
      the.days <- day.ord[ids]

      ## just use sequence of days given if <7, if not order them
      if (length(unique(x$weekday)) < 7) {
        x[[type]] <- ordered(x[[type]], levels = factor(unique(x$weekday)))
      } else {
        x[[type]] <- ordered(x[[type]], levels = the.days)
      }
    }

    if (type == "wd") {

      ## could be missing data
      id <- which(is.na(x$wd))
      if (length(id) > 0) {
        x <- x[-id, ]
        warning(paste(
          length(id),
          "missing wind direction line(s) removed"
        ), call. = FALSE)
      }

      x[[type]] <- cut(
        x$wd,
        breaks = seq(22.5, 382.5, 45),
        labels = c(
          "NE", "E", "SE", "S", "SW", "W",
          "NW", "N"
        )
      )
      x[[type]][is.na(x[[type]])] <- "N" # for wd < 22.5

      x[[type]] <- ordered(
        x[[type]],
        levels = c(
          "N", "NE", "E",
          "SE", "S", "SW", "W", "NW"
        )
      )
    }


    if (type == "site") {
      x[[type]] <- x$site
      x[[type]] <- factor(x[[type]]) ## will get rid of any unused factor levels
    }

    if (type %in% c("dst", "bstgmt", "gmtbst")) {
      type <- "dst" ## keep it simple

      ## how to extract BST/GMT
      if (is.null(local.tz)) {
        message("missing time zone, assuming Europe/London")
        local.tz <- "Europe/London"
      }

      attr(x$date, "tzone") <- local.tz

      id.nondst <- which(as.POSIXlt(x$date)$isdst == 0)
      id.dst <- which(as.POSIXlt(x$date)$isdst == 1)

      if (any(as.POSIXlt(x$date)$isdst == -1)) {
        stop("Not possible to identify DST")
      }

      x[id.nondst, type] <- "Non-DST"
      x[id.dst, type] <- "DST"
      x[[type]] <- factor(x[[type]])
    }

    if (type == "daylight") {
      x <- cutDaylight(x, latitude, longitude, ...)
    }

    x
  }

  for (i in 1:length(type)) {
    x <- makeCond(x, type[i])
  }
  x
}

###########################################################################################
# cutDaylight function
cutDaylight <- function(x, latitude = 51.522393, longitude = -0.154700, ...) {

  ## long, hour.off

  # condition openair data by daylight
  # using date (POSIXt)
  # kr v 0.2
  #################################
  # based on noaa methods
  # http://www.srrb.noaa.gov/highlights/sunrise/calcdetails.html
  # by Chris Cornwall, Aaron Horiuchi and Chris Lehman
  #

  ######################
  # notes
  ######################
  # calculations use
  # (lat, long) position relative to sun
  # to estimate if daylight or nighttime hour
  ######################
  # solar.noon.lst, etc are factions of day
  # seconds into that day = p.time * 86400
  # so for example sunset time is
  # as.POSIXct(sunset.time.lst * 86400, origin = format(x$date, "%Y-%m-%d"))
  # (assuming you do not run into next day!)
  ######################
  # currently unsure about extremes
  # long nights and days at poles need checking
  #

  ##################
  # suggestions:
  ##################
  # local hour offset could be a lookup table linked to tz
  #

  if (!"POSIXt" %in% class(x$date)) {
    stop("required field 'date' missing or not POSIXt\n", call. = FALSE)
  }

  # local hour offset

  local.hour.offset <- as.numeric(lubridate::force_tz(x$date[1], "UTC") - x$date[1])

  ###################
  # temp functions
  ###################
  rad <- function(x) x * pi / 180
  degrees <- function(x) x * (180 / pi)

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
        ifelse(p.day <= sunrise.time.lst &
          p.day >= sunset.time.lst, FALSE, TRUE)
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

  ###############################
  # output
  ###############################
  x <- cbind(x, daylight = daylight)
}
