#' Automatic text formatting for openair
#'
#' Workhorse function that automatically applies routine text formatting to
#' common expressions and data names used in openair.
#'
#' `quickText` is routine formatting lookup table. It screens the
#' supplied character vector `text` and automatically applies formatting
#' to any recognised character sub-series. The function is used in a number of
#' `openair` functions and can also be used directly by users to format
#' text components of their own graphs (see below).
#'
#' @param text A character vector.
#' @param auto.text A logical option. The default, `TRUE`, applies
#'   `quickText` to `text` and returns the result. The alternative,
#'   `FALSE`, returns `text` unchanged. (A number of `openair`
#'   functions enable/disable `quickText` using this option.
#' @export
#' @return The function returns an expression for graphical evaluation.
#' @author Karl Ropkins.
#' @examples
#'
#'
#' # example 1
#' ## see axis formatting in an openair plot, e.g.:
#' scatterPlot(mydata, x = "no2", y = "pm10")
#'
#' # example 2
#' ## using quickText in other plots
#' plot(mydata$no2, mydata$pm10,
#'   xlab = quickText("my no2 label"),
#'   ylab = quickText("pm10 [ ug.m-3 ]")
#' )
#'
quickText <- function(text, auto.text = TRUE) {
  ## the lookup table version

  ## #return if auto.text false
  if (!auto.text) {
    return(ans <- text)
  }

  ## #return if already expression
  if (is.expression(text)) {
    return(ans <- text)
  }

  ans <- paste("expression(paste('", text, " ", sep = "")
  ans <- gsub("NO2", "' 'NO' [2] * '", ans)
  ans <- gsub("no2", "' 'NO' [2] * '", ans)
  ans <- gsub("\\bno\\b", "NO", ans)
  ans <- gsub("\\bNOX\\b", "' 'NO' [x] * '", ans)
  ans <- gsub("\\bnox\\b", "' 'NO' [x] * '", ans)
  ans <- gsub("\\bNOx\\b", "' 'NO' [x] * '", ans)
  ans <- gsub("NH3", "' 'NH' [3] * '", ans)
  ans <- gsub("nh3", "' 'NH' [3] * '", ans)
  ans <- gsub("co ", "' 'CO ' '", ans)
  ans <- gsub("co,", "' 'CO,' '", ans)
  ans <- gsub("nmhc", "' 'NMHC' '", ans)

  ans <- if (nchar(as.character(text)) == 2 && length(grep("ws", text)) > 0) {
    gsub("ws", "' 'wind spd.' '", ans)
  } else {
    ans
  }
  ans <- gsub("wd", "' 'wind dir.' '", ans)
  ans <- gsub("rh ", "' 'relative humidity' '", ans)
  ans <- gsub("PM10", "' 'PM' [10] * '", ans)
  ans <- gsub("pm10", "' 'PM' [10] * '", ans)
  ans <- gsub("pm1", "' 'PM' [1] * '", ans)
  ans <- gsub("PM1", "' 'PM' [1] * '", ans)
  ans <- gsub("PM4", "' 'PM' [4] * '", ans)
  ans <- gsub("pm4", "' 'PM' [4] * '", ans)
  ans <- gsub("PMtot", "' 'PM' [total] * '", ans)
  ans <- gsub("pmtot", "' 'PM' [total] * '", ans)
  ans <- gsub("pmc", "' 'PM' [coarse] * '", ans)

  ans <- gsub("pmcoarse", "' 'PM' [coarse] * '", ans)
  ans <- gsub("PMc", "' 'PM' [coarse] * '", ans)
  ans <- gsub("PMcoarse", "' 'PM' [coarse] * '", ans)
  ans <- gsub("pmf", "' 'PM' [fine] * '", ans)
  ans <- gsub("pmfine", "' 'PM' [fine] * '", ans)
  ans <- gsub("PMf", "' 'PM' [fine] * '", ans)
  ans <- gsub("PMfine", "' 'PM' [fine] * '", ans)
  ans <- gsub("PM2.5", "' 'PM' [2.5] * '", ans)
  ans <- gsub("pm2.5", "' 'PM' [2.5] * '", ans)
  ans <- gsub("pm25", "' 'PM' [2.5] * '", ans)
  ans <- gsub("PM2.5", "' 'PM' [2.5] * '", ans)
  ans <- gsub("PM25", "' 'PM' [2.5] * '", ans)
  ans <- gsub("pm25", "' 'PM' [2.5] * '", ans)
  ans <- gsub("O3", "' 'O' [3] * '", ans)
  ans <- gsub("o3", "' 'O' [3] * '", ans)
  ans <- gsub("ozone", "' 'O' [3] * '", ans)
  ans <- gsub("CO2", "' 'CO' [2] * '", ans)
  ans <- gsub("co2", "' 'CO' [2] * '", ans)
  ans <- gsub("SO2", "' 'SO' [2] * '", ans)
  ans <- gsub("so2", "' 'SO' [2] * '", ans)
  ans <- gsub("H2S", "' 'H' [2] * 'S''", ans)
  ans <- gsub("h2s", "' 'H' [2] * 'S''", ans)
  ans <- gsub("CH4", "' 'CH' [4] * '", ans)
  ans <- gsub("ch4", "' 'CH' [4] * '", ans)
  ans <- gsub("dgrC", "' * degree * 'C' '", ans)
  ans <- gsub("degreeC", "' * degree * 'C' '", ans)
  ans <- gsub("deg. C", "' * degree * 'C' '", ans)
  ans <- gsub("degreesC", "' * degree * 'C' '", ans)
  ans <- gsub("degrees", "' * degree *'", ans)
  ans <- gsub("Delta", "' * Delta *'", ans)
  ans <- gsub("delta", "' * Delta *'", ans)
  ans <- gsub("ug/m3", "' * mu * 'g m' ^-3 *'", ans)
  ans <- gsub("ug.m-3", "' * mu * 'g m' ^-3 *'", ans)
  ans <- gsub("ug m-3", "' * mu * 'g m' ^-3 *'", ans)
  ans <- gsub("ugm-3", "' * mu * 'g m' ^-3 *'", ans)
  ans <- gsub("mg/m3", "' * 'm' * 'g m' ^-3 *'", ans)
  ans <- gsub("mg.m-3", "' * 'm' * 'g m' ^-3 *'", ans)
  ans <- gsub("mg m-3", "' * 'm' * 'g m' ^-3 *'", ans)
  ans <- gsub("mgm-3", "' * 'm' * 'g m' ^-3 *'", ans)
  ans <- gsub("ng/m3", "' * 'n' * 'g m' ^-3 *'", ans)
  ans <- gsub("ng.m-3", "' * 'n' * 'g m' ^-3 *'", ans)
  ans <- gsub("ng m-3", "' * 'n' * 'g m' ^-3 *'", ans)
  ans <- gsub("ngm-3", "' * 'n' * 'g m' ^-3 *'", ans)
  ans <- gsub("m/s2", "' 'm s' ^-2 *'", ans)
  ans <- gsub("m/s", "' 'm s' ^-1 *'", ans)
  ans <- gsub("m.s-1", "' 'm s' ^-1 *'", ans)
  ans <- gsub("m s-1", "' 'm s' ^-1 *'", ans)
  ans <- gsub("km2", "' 'km' ^2 *'", ans)
  ans <- gsub("g/km", "' 'g km' ^-1 *'", ans)
  ans <- gsub("g/s", "' 'g s' ^-1 *'", ans)
  ans <- gsub("kW/t", "' 'kW t' ^-1 *'", ans)
  ans <- gsub("g/hour", "' 'g hour' ^-1 *'", ans)
  ans <- gsub("g/hr", "' 'g hour' ^-1 *'", ans)
  ans <- gsub("g/m3", "' 'g m' ^-3 *'", ans)
  ans <- gsub("g/kg", "' 'g kg' ^-1 *'", ans)
  ans <- gsub("cm-3", "' 'cm' ^-3 *'", ans)
  ans <- gsub("km/hr/s", "' 'km hr' ^-1 * ' s' ^-1 *'", ans)
  ans <- gsub("km/hour/s", "' 'km hr' ^-1 * ' s' ^-1 *'", ans)
  ans <- gsub("km/h/s", "km hr' ^-1 * ' s' ^-1 *'", ans)
  ans <- gsub("km/hr", "' 'km hr' ^-1 *'", ans)
  ans <- gsub("km/h", "' 'km hr' ^-1 *'", ans)
  ans <- gsub("km/hour", "' 'km hr' ^-1 *'", ans)

  ans <- gsub("r2", "R' ^2 *'", ans)
  ans <- gsub("R2", "R' ^2 *'", ans)

  ans <- gsub("tau ", "' * tau * '", ans)

  ans <- gsub("umol/m2/s", "' * mu * 'mol m' ^-2 * ' s' ^-1 *'", ans)
  ans <- gsub("umol/m2", "' * mu * 'mol m' ^-2 *'", ans)

  ans <- paste(ans, "'))", sep = "")

  ## commands to strip unecessary * etc...

  if (substr(ans, (nchar(ans) - 8), (nchar(ans) - 6)) == "] *") {
    a <- ans
    ans <- paste(
      substr(a, 1, (nchar(a) - 7)),
      substr(a, (nchar(a) - 5), nchar(a)),
      sep = ""
    )
  }

  ans <- gsub("''", "", ans)
  ans <- gsub("' '", "", ans)
  ans <- gsub("\\*  \\*", "~", ans)
  ans <- gsub("^expression\\(paste\\( \\*", "expression(paste(", ans)
  ans <- gsub("^expression\\(paste\\(\\*", "expression(paste(", ans)

  if (substr(ans, (nchar(ans) - 2), (nchar(ans) - 2)) == "*") {
    a <- ans
    ans <- paste(
      substr(a, 1, (nchar(a) - 2)),
      " ' ' ",
      substr(a, (nchar(a) - 1), nchar(a)),
      sep = ""
    )
  }

  ## ###################
  ## new bit
  ## replace a \n b with atop(a,b)
  ## one newline only

  if (grepl("\n", ans)) {
    a <- ans
    ans <- paste(substr(a, 1, 17), "atop(", substr(a, 18, nchar(a)), sep = "")
    ans <- gsub("\n", "' , '", ans)
    temp <- paste(")", sep = "", collapse = "")
    ans <- paste(ans, temp, sep = "")
  }

  ## ########################

  if (inherits(try(eval(parse(text = ans)), TRUE), "try-error") == FALSE) {
    ans <- eval(parse(text = ans))
  } else {
    ans <- text
  }
}
