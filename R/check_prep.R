#' Check input file and prepare data
#'
#' Workhorse used throughout openair to check data is in an appropriate state
#' for further analysis & plotting.
#'
#' @param mydata Input data from parent function
#'
#' @param vars The variables needed by the parent function. `mydata` is reduced
#'   to these columns, plus any `type` columns.
#'
#' @param type The conditioning variable(s) used by the parent function. These
#'   must either be columns of `mydata` or one of the built-in date-based types.
#'
#' @param remove_calms If `TRUE`, sets "calm" (`ws == 0`) `ws` and `wd` values
#'   to `NA`, and rounds `wd` to the nearest 10 degrees.
#'
#' @param remove_negative_ws If `TRUE`, sets negative wind speeds to `NA`.
#'
#' @param ws,wd The names of the wind speed and wind direction columns.
#'
#' @param require_data If `TRUE`, error if, after preparation, `mydata` has no
#'   rows or any of `vars` (other than `"date"`) contains only missing values.
#'   Used by plotting functions, which otherwise fail with unhelpful errors or
#'   return empty plots.
#'
#' @param call The execution environment of the calling function, used to
#'   report errors from `require_data`.
#'
#' @author David Carslaw
#' @noRd
check_prep <- function(
  mydata,
  vars,
  type,
  remove_calms = FALSE,
  remove_negative_ws = TRUE,
  ws = "ws",
  wd = "wd",
  require_data = FALSE,
  call = rlang::caller_env()
) {
  # ensure data is ungrouped
  mydata <- dplyr::ungroup(mydata)

  # variables to check for data if `require_data = TRUE`; recorded before
  # `vars` has any `type` columns added
  data_vars <- setdiff(vars, "date")

  # deal with conditioning variable if present, if user-defined, must exist in
  # data pre-defined types existing conditioning variables that only depend on
  # date (which is checked)
  conds <- c("default", dateTypes)
  all_vars <- unique(c(names(mydata), conds))

  # names we want to be there
  var_names <- c(vars, type)
  matching <- var_names %in% all_vars

  if (!all(matching)) {
    cli::cli_abort(
      "Can't find the variable(s): {.field {var_names[!matching]}}.",
      call = call
    )
  }

  # keep any `type` columns already in the data (user-defined types must be,
  # as checked above; built-in date-based types are created later by cutData)
  vars <- unique(c(vars, intersect(type, names(mydata))))

  # just select data needed; `[` (not `[, ]`) so a single column stays a data
  # frame
  mydata <- mydata[vars]

  # remove rows with a missing site
  if ("site" %in% names(mydata)) {
    mydata <- mydata[!is.na(mydata$site), ]
  }

  # sometimes ratios are considered which can results in infinite values
  # make sure all infinite values are set to NA
  mydata[] <- lapply(mydata, function(x) {
    if (is.numeric(x)) {
      x[is.infinite(x)] <- NA
    }
    x
  })

  # remove negative wind speeds
  if (
    remove_negative_ws &&
      ws %in% vars &&
      is.numeric(mydata[[ws]]) &&
      any(mydata[[ws]] < 0, na.rm = TRUE)
  ) {
    cli::cli_warn("Wind speed < 0; removing negative data", call = call)
    mydata[[ws]][mydata[[ws]] < 0] <- NA
  }

  # round wd to make processing obvious
  # data already rounded to nearest 10 degress will not be affected
  # data not rounded will be rounded to nearest 10 degrees
  # assumes 10 is average of 5-15 etc
  if (wd %in% vars && is.numeric(mydata[[wd]])) {
    # check for wd < 0 or > 360
    if (any(mydata[[wd]] < 0 | mydata[[wd]] > 360, na.rm = TRUE)) {
      cli::cli_warn(
        "Wind direction < 0 or > 360; removing these data.",
        call = call
      )
      mydata[[wd]][mydata[[wd]] < 0 | mydata[[wd]] > 360] <- NA
    }

    if (remove_calms) {
      if (ws %in% names(mydata)) {
        mydata[[wd]][mydata[[ws]] == 0] <- NA # set wd to NA where there are calms
        mydata[[ws]][mydata[[ws]] == 0] <- NA # remove calm ws
      }

      # round wd for use in functions - except windRose/pollutionRose
      mydata[[wd]] <- 10 * ceiling(mydata[[wd]] / 10 - 0.5)
    }

    # north is 360, not 0 (incl. angles < 5 rounded down above)
    mydata[[wd]][mydata[[wd]] == 0] <- 360
  }

  # make sure date is ordered in time if present
  if ("date" %in% vars) {
    # check if date is Date or POSIXct
    if (!inherits(mydata$date, "Date") && !inherits(mydata$date, "POSIXct")) {
      cli::cli_abort(
        c(
          "x" = "{.code mydata$date} is {.type {mydata$date}}.",
          "i" = "{.code mydata$date} must be {.type {as.Date(ISOdate(1,1,1))}} or {.type {ISOdate(1,1,1)}}."
        ),
        call = call
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
        call = call
      )
    }

    # daylight saving time can cause terrible problems - best avoided!!
    if (any(lubridate::dst(mydata$date))) {
      cli::cli_inform(
        c("i" = "Detected data with Daylight Saving Time."),
        call = call
      )
    }
  }

  # check there is something to plot, after any data have been removed above
  if (require_data) {
    check_has_data(mydata, data_vars, call = call)
  }

  # return data frame
  return(mydata)
}
