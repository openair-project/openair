#' Calculate rollingMean values
#'
#' Calculate rollingMean values taking account of data capture thresholds
#'
#' This is a utility function mostly designed to calculate rolling
#' mean statistics relevant to some pollutant limits e.g. 8 hour
#' rolling means for ozone and 24 hour rolling means for
#' PM10. However, the function has a more general use in helping to
#' display rolling mean values in flexible ways e.g. with the rolling
#' window width left, right or centre aligned.
#'
#' The function will try and fill in missing time gaps to get a full
#' time sequence but return a data frame with the same number of rows
#' supplied.
#'
#' @param mydata A data frame containing a \code{date}
#' field. \code{mydata} must contain a \code{date} field in
#' \code{Date} or \code{POSIXct} format. The input time series must
#' be regular e.g. hourly, daily.
#' @param pollutant The name of a pollutant e.g. \code{pollutant = "o3"}.
#' @param width The averaging period (rolling window width) to use
#' e.g. \code{width = 8} will generate 8-hour rolling mean values
#' when hourly data are analysed.
#' @param new.name The name given to the new rollingMean variable. If
#' not supplied it will create a name based on the name of the
#' pollutant and the averaging period used.
#' @param data.thresh The data capture threshold in %. No values are
#' calculated if data capture over the period of interest is less
#' than this value. For example, with \code{width = 8} and
#' \code{data.thresh = 75} at least 6 hours are required to calculate
#' the mean, else \code{NA} is returned.
#' @param align specifies how the moving window should be
#' aligned. \code{"right"} means that the previous \code{hours}
#' (including the current) are averaged. This seems to be the default
#' for UK air quality rolling mean statistics. \code{"left"} means
#' that the forward \code{hours} are averaged, and \code{"centre"} or
#' \code{"center"}, which is the default.
#' @param ... other arguments, currently unused.
#' @export
#' @author David Carslaw
#' @examples
#'
#' ## rolling 8-hour mean for ozone
#' mydata <- rollingMean(mydata, pollutant = "o3", width = 8, new.name =
#' "rollingo3", data.thresh = 75, align = "right")
#'
#'
rollingMean <- function(mydata,
                        pollutant = "o3",
                        width = 8,
                        new.name = "rolling",
                        data.thresh = 75,
                        align = "centre",
                        ...) {
  ## function to calculate rolling means
  ## uses C++ code
  
  ## get rid of R check annoyances
  site <- NULL
  if (!align %in% c("left", "right", "centre", "center"))
    stop("align should be one of 'right', 'left', 'centre' or 'center'.")
  
  
  if (missing(new.name))
    new.name <- paste("rolling", width, pollutant, sep = "")
  if (data.thresh < 0 |
      data.thresh > 100)
    stop("Data threshold must be between 0 and 100.")
  
  calc.rolling <- function(mydata, ...) {
    ## data needs to be numeric
    if (!is.numeric(mydata[[pollutant]])) {
      warning("Data are not numeric.")
      return(mydata)
    }
    
    ## need to know whether dates added
    dates <- mydata$date
    
    ## pad missing hours
    mydata <- date.pad(mydata)
    
    ## make sure function is not called with window width longer than data
    if (width > nrow(mydata))
      return(mydata)
    
    mydata[[new.name]] <- .Call("rollMean", mydata[[pollutant]], width, data.thresh, align, PACKAGE = "openair")
    
    if (length(dates) != nrow(mydata)) {
      ## return what was put in
      ## avoids adding missing data e.g. for factors
      mydata <- mydata[mydata$date %in% dates, ]
    }
    
    mydata
  }
  
  ## split if several sites
  if ("site" %in% names(mydata)) {
    ## split by site
    
    mydata <- group_by(mydata, site) %>%
      do(calc.rolling(., ...))
    
    mydata
  } else {
    mydata <- calc.rolling(mydata, ...)
    mydata
  }
}
