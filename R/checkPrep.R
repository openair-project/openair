#' Check input file and prepare data
#'
#' Workhorse used throughout openair to check data is in an appropriate state
#' for further analysis & plotting.
#'
#' @param mydata Input data from parent function
#'
#' @param remove.calm If `TRUE`, sets "calm" (ws == 0) wd/ws cols to `NA`, and
#'   rounds `wd` for plotting.
#'
#' @param remove.neg If `TRUE` sets negative wind speeds to NA.
#'
#' @param wd Varname for wind direction.
#'
#' @author David Carslaw
#' @noRd
checkPrep <- function(
  mydata,
  Names,
  type,
  remove.calm = TRUE,
  remove.neg = TRUE,
  wd = "wd"
) {
  # deal with conditioning variable if present, if user-defined, must exist in
  # data pre-defined types existing conditioning variables that only depend on
  # date (which is checked)
  conds <- c("default", dateTypes)
  all_vars <- unique(c(names(mydata), conds))

  # names we want to be there
  var_names <- c(Names, type)
  matching <- var_names %in% all_vars

  if (!all(matching)) {
    cli::cli_abort(
      "Can't find the variable(s): {.field {var_names[!matching]}}.",
      call = NULL
    )
  }

  # add type to names if not in pre-defined list
  if (!all(type %in% conds)) {
    ids <- which(!type %in% conds)
    Names <- c(Names, type[ids])
  }

  # if type already present in data frame
  if (any(type %in% names(mydata))) {
    ids <- which(type %in% names(mydata))
    Names <- unique(c(Names, type[ids]))
  }

  # just select data needed
  mydata <- mydata[, Names]

  # if site is in the data set, check none are missing
  # seems to be a problem for some KCL data...
  if ("site" %in% names(mydata)) {
    # split by site

    # remove any NA sites
    if (anyNA(mydata$site)) {
      id <- which(is.na(mydata$site))
      mydata <- mydata[-id, ]
    }
  }

  # sometimes ratios are considered which can results in infinite values
  # make sure all infinite values are set to NA
  mydata[] <- lapply(mydata, function(x) {
    replace(x, x == Inf | x == -Inf, NA)
  })

  if ("ws" %in% Names) {
    if ("ws" %in% Names && is.numeric(mydata$ws)) {
      # check for negative wind speeds
      if (any(sign(mydata$ws[!is.na(mydata$ws)]) == -1)) {
        if (remove.neg) {
          # remove negative ws only if TRUE
          cli::cli_warn("Wind speed < 0; removing negative data", call = NULL)
          mydata$ws[mydata$ws < 0] <- NA
        }
      }
    }
  }

  # round wd to make processing obvious
  # data already rounded to nearest 10 degress will not be affected
  # data not rounded will be rounded to nearest 10 degrees
  # assumes 10 is average of 5-15 etc
  if (wd %in% Names) {
    if (wd %in% Names && is.numeric(mydata[, wd])) {
      # check for wd <0 or > 360
      if (
        any(
          sign(mydata[[wd]][!is.na(mydata[[wd]])]) == -1 |
            mydata[[wd]][!is.na(mydata[[wd]])] > 360
        )
      ) {
        cli::cli_warn(
          "Wind direction < 0 or > 360; removing these data.",
          call = NULL
        )
        mydata[[wd]][mydata[[wd]] < 0] <- NA
        mydata[[wd]][mydata[[wd]] > 360] <- NA
      }

      if (remove.calm) {
        if ("ws" %in% names(mydata)) {
          mydata[[wd]][mydata$ws == 0] <- NA # set wd to NA where there are calms
          mydata$ws[mydata$ws == 0] <- NA # remove calm ws
        }
        mydata[[wd]][mydata[[wd]] == 0] <- 360 # set any legitimate wd to 360

        # round wd for use in functions - except windRose/pollutionRose
        mydata[[wd]] <- 10 * ceiling(mydata[[wd]] / 10 - 0.5)
        mydata[[wd]][mydata[[wd]] == 0] <- 360 # angles <5 should be in 360 bin
      }
      mydata[[wd]][mydata[[wd]] == 0] <- 360 # set any legitimate wd to 360
    }
  }

  # make sure date is ordered in time if present
  if ("date" %in% Names) {
    # check if date is Date or POSIXct
    if (!inherits(mydata$date, "Date") && !inherits(mydata$date, "POSIXct")) {
      cli::cli_abort(
        c(
          "x" = "{.field mydata$date} is {.type {mydata$date}}.",
          "i" = "{.field mydata$date} must be {.type {as.Date(ISOdate(1,1,1))}} or {.type {ISOdate(1,1,1)}}."
        ),
        call = NULL
      )
    }

    # sort by date
    mydata <- dplyr::arrange(mydata, date)

    # make sure date is the first field
    if (names(mydata)[1] != "date") {
      mydata <- mydata[c("date", setdiff(names(mydata), "date"))]
    }

    # check to see if there are any missing dates, stop if there are
    ids <- which(is.na(mydata$date))
    if (length(ids) > 0) {
      mydata <- mydata[-ids, ]
      cli::cli_warn(
        "Missing dates detected, removing {length(ids)} lines.",
        call = NULL
      )
    }

    # daylight saving time can cause terrible problems - best avoided!!
    if (any(lubridate::dst(mydata$date))) {
      cli::cli_inform(
        c("i" = "Detected data with Daylight Saving Time."),
        call = NULL
      )
    }
  }

  # return data frame
  return(mydata)
}
