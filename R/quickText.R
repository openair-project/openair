# Vectorised `quickText()`
label_openair <- function(x, parse = TRUE, auto_text = TRUE) {
  if (!auto_text) {
    return(x)
  }

  sapply(
    x,
    \(txt) quickText(txt, auto.text = auto_text, parse = parse)
  )
}

# Return a labeller for facets
labeller_openair <- function(auto_text = TRUE) {
  if (auto_text) {
    ggplot2::as_labeller(
      \(x) label_openair(x, parse = FALSE, auto_text = TRUE),
      default = ggplot2::label_parsed
    )
  } else {
    ggplot2::label_value
  }
}

#' Automatic text formatting for openair
#'
#' Workhorse function that automatically applies routine text formatting to
#' common expressions and data names used in openair.
#'
#' [quickText()] is routine formatting lookup table. It screens the supplied
#' character vector `text` and automatically applies formatting to any
#' recognised character sub-series. The function is used in a number of
#' `openair` functions and can also be used directly by users to format text
#' components of their own graphs (see below).
#'
#' @param text A character vector.
#' @param auto.text A logical option. The default, `TRUE`, applies [quickText()]
#'   to `text` and returns the result. The alternative, `FALSE`, returns `text`
#'   unchanged. (A number of `openair` functions enable/disable [quickText()]
#'   using this option).
#' @param ... Not used.
#' @export
#' @return The function returns an expression for graphical evaluation.
#' @author Karl Ropkins
#' @author David Carslaw
#' @author Jack Davison
#' @examples
#'
#' # see axis formatting in an openair plot, e.g.:
#' scatterPlot(
#'   mydata,
#'   x = "no2",
#'   y = "pm10"
#' )
#'
#' # using quickText in other plots
#' plot(
#'   mydata$no2,
#'   mydata$pm10,
#'   xlab = quickText("my no2 label"),
#'   ylab = quickText("pm10 [ ug.m-3 ]")
#' )
quickText <- function(text, auto.text = TRUE, ...) {
  ## the lookup table version

  ## #return if auto.text false
  if (!auto.text) {
    ans <- text
    return(ans)
  }

  # are we going to actually parse this?
  extra_args <- capture_dots(...)
  parse <- extra_args$parse %||% TRUE

  ## #return if already expression
  if (is.expression(text)) {
    ans <- text
    return(ans)
  }

  # escape backslashes so `text` can be safely embedded in a string within the
  # plotmath code. Quotes are swapped for a placeholder so that the clean-up of
  # empty strings (e.g., `''`) below can't remove them; they are escaped
  # afterwards
  quote_placeholder <- "\u001f"
  text_escaped <- gsub("\\\\", "\\\\\\\\", text)
  text_escaped <- gsub("'", quote_placeholder, text_escaped, fixed = TRUE)

  if (parse) {
    ans <- paste("expression(paste('", text_escaped, sep = "")
  } else {
    ans <- paste("paste('", text_escaped, sep = "")
  }

  # substitute `x` only when it isn't part of a longer word, i.e., not preceded
  # by a letter and not followed by a letter or digit. This prevents, e.g.,
  # "year2020" becoming "yeaR^2 020" or "pm100" becoming "PM10 0". Underscores
  # are allowed either side so names like "no2_roadside" are still formatted.
  sub_word <- function(pattern, replacement, x) {
    gsub(
      paste0("(?<![A-Za-z])", pattern, "(?![A-Za-z0-9])"),
      replacement,
      x,
      perl = TRUE
    )
  }

  ans <- sub_word("NO2", "' 'NO' [2] * '", ans)
  ans <- sub_word("no2", "' 'NO' [2] * '", ans)
  ans <- sub_word("NO3", "' 'NO' [3] * '", ans)
  ans <- sub_word("no3", "' 'NO' [3] * '", ans)
  ans <- gsub("\\bno\\b", "NO", ans)
  ans <- gsub("\\bNOX\\b", "' 'NO' [x] * '", ans)
  ans <- gsub("\\bnox\\b", "' 'NO' [x] * '", ans)
  ans <- gsub("\\bNOx\\b", "' 'NO' [x] * '", ans)
  ans <- sub_word("NH3", "' 'NH' [3] * '", ans)
  ans <- sub_word("nh3", "' 'NH' [3] * '", ans)
  ans <- sub_word("co", "' 'CO' '", ans)
  ans <- sub_word("nmhc", "' 'NMHC' '", ans)
  ans <- gsub("\\bws\\b", "' 'wind spd.' '", ans)
  ans <- gsub("\\bwd\\b", "' 'wind dir.' '", ans)
  ans <- gsub("\\bWS\\b", "' 'wind spd.' '", ans)
  ans <- gsub("\\bWD\\b", "' 'wind dir.' '", ans)
  ans <- gsub("air_temp", "' 'temperature' '", ans)
  ans <- sub_word("rh", "' 'relative humidity' '", ans)
  ans <- sub_word("PM10", "' 'PM' [10] * '", ans)
  ans <- sub_word("pm10", "' 'PM' [10] * '", ans)
  ans <- sub_word("pm1", "' 'PM' [1] * '", ans)
  ans <- sub_word("PM1", "' 'PM' [1] * '", ans)
  ans <- sub_word("PM4", "' 'PM' [4] * '", ans)
  ans <- sub_word("pm4", "' 'PM' [4] * '", ans)
  ans <- sub_word("PMtot", "' 'PM' [total] * '", ans)
  ans <- sub_word("pmtot", "' 'PM' [total] * '", ans)
  # long forms must come before short forms (e.g., "pmcoarse" before "pmc"),
  # otherwise the short form matches first and leaves the rest of the word
  ans <- sub_word("pmcoarse", "' 'PM' [coarse] * '", ans)
  ans <- sub_word("PMcoarse", "' 'PM' [coarse] * '", ans)
  ans <- sub_word("pmc", "' 'PM' [coarse] * '", ans)
  ans <- sub_word("PMc", "' 'PM' [coarse] * '", ans)
  ans <- sub_word("pmfine", "' 'PM' [fine] * '", ans)
  ans <- sub_word("PMfine", "' 'PM' [fine] * '", ans)
  ans <- sub_word("pmf", "' 'PM' [fine] * '", ans)
  ans <- sub_word("PMf", "' 'PM' [fine] * '", ans)
  ans <- sub_word("PM2\\.5", "' 'PM' [2.5] * '", ans)
  ans <- sub_word("pm2\\.5", "' 'PM' [2.5] * '", ans)
  ans <- sub_word("pm25", "' 'PM' [2.5] * '", ans)
  ans <- sub_word("PM2\\.5", "' 'PM' [2.5] * '", ans)
  ans <- sub_word("PM25", "' 'PM' [2.5] * '", ans)
  ans <- sub_word("pm25", "' 'PM' [2.5] * '", ans)
  ans <- sub_word("O3", "' 'O' [3] * '", ans)
  ans <- sub_word("o3", "' 'O' [3] * '", ans)
  ans <- sub_word("ozone", "' 'O' [3] * '", ans)
  ans <- sub_word("CO2", "' 'CO' [2] * '", ans)
  ans <- sub_word("co2", "' 'CO' [2] * '", ans)
  ans <- sub_word("SO2", "' 'SO' [2] * '", ans)
  ans <- sub_word("so2", "' 'SO' [2] * '", ans)
  ans <- sub_word("H2S", "' 'H' [2] * 'S''", ans)
  ans <- sub_word("h2s", "' 'H' [2] * 'S''", ans)
  ans <- sub_word("CH4", "' 'CH' [4] * '", ans)
  ans <- sub_word("ch4", "' 'CH' [4] * '", ans)
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

  ans <- sub_word("r2", "R' ^2 *'", ans)
  ans <- sub_word("R2", "R' ^2 *'", ans)

  ans <- sub_word("tau", "' * tau * '", ans)

  ans <- gsub("umol/m2/s", "' * mu * 'mol m' ^-2 * ' s' ^-1 *'", ans)
  ans <- gsub("umol/m2", "' * mu * 'mol m' ^-2 *'", ans)

  if (parse) {
    ans <- paste(ans, "'))", sep = "")
  } else {
    ans <- paste(ans, "')", sep = "")
  }

  ## commands to strip unecessary * etc...

  ans <- gsub("''", "", ans)
  # a space between a formatted name/unit and a following one (e.g., "ug/m3
  # no2") is left as a string containing only spaces (`*' ' 'NO'`). Make it a
  # plotmath space (`~`) before stray `' '` strings are removed below,
  # otherwise the space is lost (e.g., giving "ug m-3NO2")
  ans <- gsub("\\*\\s*' +'\\s*'", "~ '", ans)
  ans <- gsub("' '", "", ans)
  ans <- gsub("\\*  \\*", "~", ans)
  # a leading `*` (e.g., before a symbol) is invalid plotmath, for both
  # `parse = TRUE` ("expression(paste(") and `parse = FALSE` ("paste(")
  ans <- sub("^(expression\\()?paste\\( ?\\*", "\\1paste(", ans)

  # a trailing `*` (e.g., after a unit) is invalid plotmath, so remove it.
  # Matched as a pattern, as the closing brackets differ between `parse = TRUE`
  # ("))") and `parse = FALSE` (")") and may follow a space
  ans <- sub("\\s*\\*(\\s*\\)+)$", "\\1", ans)

  # two `*` with nothing between them (e.g., from adjacent symbols like "Delta
  # Delta" or "ug/m3 ug/m3") are invalid plotmath, or parse as a power (`**`),
  # so join them with a space (`~`) instead
  ans <- gsub("\\*\\s*\\*", "~", ans)

  # now restore the user's quotes, escaped
  ans <- gsub(quote_placeholder, "\\'", ans, fixed = TRUE)

  ## ###################
  ## new bit
  ## replace a \n b with atop(a,b)
  ## one newline only

  if (grepl("\n", ans)) {
    if (parse) {
      a <- ans
      ans <- paste(substr(a, 1, 17), "atop(", substr(a, 18, nchar(a)), sep = "")
      ans <- gsub("\n", "' , '", ans)
      temp <- paste(")", sep = "", collapse = "")
      ans <- paste(ans, temp, sep = "")
    } else {
      ans <- gsub("\n", " ", ans)
    }
  }

  ## ########################

  # if the expression can't be built, fall back to the original text
  if (parse) {
    ans <- tryCatch(eval(parse(text = ans)), error = function(e) text)
  }

  return(ans)
}
