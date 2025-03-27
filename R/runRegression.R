#' Rolling regression for pollutant source characterisation.
#'
#' This function calculates rolling regressions for input data with a set window
#' width. The principal use of the function is to identify "dilution lines"
#' where the ratio between two pollutant concentrations is invariant. The
#' original idea is based on the work of Bentley (2004).
#'
#' The intended use is to apply the approach to air pollution data to extract
#' consecutive points in time where the ratio between two pollutant
#' concentrations changes by very little. By filtering the output for high R2
#' values (typically more than 0.90 to 0.95), conditions where local source
#' dilution is dominant can be isolated for post processing. The function is
#' more fully described and used in the `openair` online manual, together
#' with examples.
#'
#' @param mydata A data frame with  columns for `date` and at least two
#'   variables for use in a regression.
#' @param x The column name of the `x` variable for use in a linear
#'   regression `y = m.x + c`.
#' @param y The column name of the `y` variable for use in a linear
#'   regression `y = m.x + c`.
#' @param run.len The window width to be used for a rolling regression. A value
#'   of 3 for example for hourly data will consider 3 one-hour time sequences.
#' @param date.pad Should gaps in time series be filled before calculations are
#'   made?
#' @return A tibble with `date` and calculated regression coefficients and
#'   other information to plot dilution lines.
#' @importFrom stats coefficients
#' @importFrom stats .lm.fit
#' @export
#' @family time series and trend functions
#' @references
#'
#'
#' For original inspiration:
#'
#' Bentley, S. T. (2004). Graphical techniques for constraining estimates of
#' aerosol emissions from motor vehicles using air monitoring network data.
#' Atmospheric Environment,(10), 1491–1500.
#' https://doi.org/10.1016/j.atmosenv.2003.11.033
#'
#' Example for vehicle emissions high time resolution data:
#'
#' Farren, N. J., Schmidt, C., Juchem, H., Pöhler, D., Wilde, S. E., Wagner, R.
#' L., Wilson, S., Shaw, M. D., & Carslaw, D. C. (2023). Emission ratio
#' determination from road vehicles using a range of remote emission sensing
#' techniques. Science of The Total Environment, 875.
#' https://doi.org/10.1016/j.scitotenv.2023.162621.
#'
#' @examples
#' # Just use part of a year of data
#' output <- runRegression(selectByDate(mydata, year = 2004, month = 1:3),
#' x = "nox", y = "pm10", run.len = 3)
#'
#' output
runRegression <- function(mydata, x = "nox", y = "pm10", run.len = 3,
                          date.pad = TRUE) {
  ## think about it in terms of y = fn(x) e.g. pm10 = a.nox + b

  vars <- c("date", x, y)

  mydata <- checkPrep(mydata, vars, type = "default")

  ## pad missing data
  if (date.pad)
    mydata <- date.pad(mydata)

  # list of rolling data frames
  mydata <-
    lapply(seq_len(nrow(mydata) - run.len + 1), function(i) {
      mydata[i:(i + run.len - 1), ]
    })

  ## select non-missing with run.len rows

  mydata <-
    mydata[which(lapply(mydata, function(x) {
      nrow(na.omit(x))
    }) == run.len)]

  model <- function(df) {
   # lm(eval(paste(y, "~", x)), data = df)
    # fast model fitting
    fit <- .lm.fit(cbind(1, df[[x]]),df[[y]])

    r.sq <- 1 - var(fit$residuals) / var(df[[y]])

    if (all(fit$residuals < 1e-7)) r.sq <- 1

    fit$r.sq <-  r.sq
    return(fit)

  }

  # suppress warnings (perfect fit)
  rsq <- function(x) {
    tryCatch(summary(x)$r.squared, warning = function(w) return(1))
  }

  # und models
  models <- lapply(mydata, model)

  # extract components
  slope <- models %>%
    map(coefficients) %>%
    map_dbl(2)

  intercept <- models %>%
    map(coefficients) %>%
    map_dbl(1)

 # r_squared <- models %>% map_dbl(rsq)
  r_squared <- models %>% map("r.sq") %>% map_dbl(1)
  date <- mydata %>% map_vec(~ median(.x$date)) # use median date
  date_start <- mydata %>% map_vec(~ min(.x$date))
  date_end <- mydata %>% map_vec(~ max(.x$date))

  results <- tibble(date, date_start, date_end, intercept, slope, r_squared)

  # info for regression lines
  x1 <- mydata %>%
    map_dbl(~ min(.x[[x]]))

  x2 <- mydata %>%
    map_dbl(~ max(.x[[x]]))

  results <- cbind(results, x1, x2)

  results <- results %>%
    mutate(
      y1 = slope * x1 + intercept,
      y2 = slope * x2 + intercept,
      delta_x = x2 - x1,
      delta_y = y2 - y1
    )

  names(results)[names(results) == "x1"] <- paste0(x, "_1")
  names(results)[names(results) == "x2"] <- paste0(x, "_2")
  names(results)[names(results) == "y1"] <- paste0(y, "_1")
  names(results)[names(results) == "y2"] <- paste0(y, "_2")
  names(results)[names(results) == "delta_x"] <- paste0("delta_", x)
  names(results)[names(results) == "delta_y"] <- paste0("delta_", y)

  as_tibble(results)
}
