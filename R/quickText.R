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
  if (!auto.text) {
    return(text)
  }

  # `parse = FALSE` returns plotmath code as a string, e.g., for
  # `ggplot2::label_parsed()`
  parse <- capture_dots(...)$parse %||% TRUE

  if (is.expression(text)) {
    return(text)
  }

  # coerce anything else (e.g., `NULL`, `NA`, numbers, factors) to character
  text <- paste(text)
  if (length(text) == 0L) {
    text <- ""
  }

  out <- lapply(text, quicktext_format, parse = parse)

  if (parse) {
    as.expression(out)
  } else {
    unlist(out)
  }
}

# Formatting rules ---------------------------------------------------------

# Each rule replaces text matching a regular expression `pattern` with a
# plotmath `expr`. `boundary` controls where a pattern may match:
#
#   - "name": not preceded by a letter, nor followed by a letter or digit. This
#   stops, e.g., "pm100" or "year2020" being formatted, but allows underscores
#   either side so that names like "no2_roadside" are formatted.
#
#   - "word": a regular expression word boundary (`\b`) either side, so
#   underscores also prevent a match (e.g., "ws_max" is not formatted).
#
#   - "none": matched anywhere, e.g., the "m/s" in "km/s".
#
# All patterns are combined into one regular expression, so each piece of text
# is matched once and replacements are never re-matched by other rules. Where
# patterns could match at the same position, the first rule (and the first
# alternative within a rule) wins, so longer forms must come first (e.g.,
# "m/s2" before "m/s"). Patterns must not contain capturing groups.
qt_rule <- function(pattern, expr, boundary = "name") {
  list(pattern = pattern, expr = expr, boundary = boundary)
}

quicktext_rules <- list(
  # gases
  qt_rule("NO2|no2", quote("NO"[2])),
  qt_rule("NO3|no3", quote("NO"[3])),
  qt_rule("NOX|NOx|nox", quote("NO"[x]), "word"),
  qt_rule("no", "NO", "word"),
  qt_rule("NH3|nh3", quote("NH"[3])),
  qt_rule("CO2|co2", quote("CO"[2])),
  qt_rule("co", "CO"),
  qt_rule("nmhc", "NMHC"),
  qt_rule("O3|o3|ozone", quote("O"[3])),
  qt_rule("SO2|so2", quote("SO"[2])),
  qt_rule("H2S|h2s", quote("H"[2] * "S")),
  qt_rule("CH4|ch4", quote("CH"[4])),

  # particulate matter
  qt_rule("PM10|pm10", quote("PM"[10])),
  qt_rule("PM1|pm1", quote("PM"[1])),
  qt_rule("PM4|pm4", quote("PM"[4])),
  qt_rule("PM2\\.5|pm2\\.5|PM25|pm25", quote("PM"[2.5])),
  qt_rule("PMtot|pmtot", quote("PM"[total])),
  qt_rule("PMcoarse|pmcoarse|PMc|pmc", quote("PM"[coarse])),
  qt_rule("PMfine|pmfine|PMf|pmf", quote("PM"[fine])),

  # meteorology
  qt_rule("WS|ws", "wind spd.", "word"),
  qt_rule("WD|wd", "wind dir.", "word"),
  qt_rule("air_temp", "temperature", "none"),
  qt_rule("rh", "relative humidity"),

  # statistics and symbols
  qt_rule("R2|r2", quote("R"^2)),
  qt_rule("tau", quote(tau)),
  qt_rule("Delta|delta", quote(Delta), "none"),
  qt_rule("degreesC|degreeC|dgrC|deg\\. C", quote(degree * "C"), "none"),
  qt_rule("degrees", quote(degree), "none"),

  # concentrations
  qt_rule("ug/m3|ug\\.m-3|ug m-3|ugm-3", quote(mu * "g m"^-3), "none"),
  qt_rule("mg/m3|mg\\.m-3|mg m-3|mgm-3", quote("mg m"^-3), "none"),
  qt_rule("ng/m3|ng\\.m-3|ng m-3|ngm-3", quote("ng m"^-3), "none"),
  qt_rule("g/m3", quote("g m"^-3), "none"),
  qt_rule("cm-3", quote("cm"^-3), "none"),
  qt_rule("umol/m2/s", quote(mu * "mol m"^-2 * " s"^-1), "none"),
  qt_rule("umol/m2", quote(mu * "mol m"^-2), "none"),

  # speeds and rates
  qt_rule("km/hour/s|km/hr/s|km/h/s", quote("km hr"^-1 * " s"^-1), "none"),
  qt_rule("km/hour|km/hr|km/h", quote("km hr"^-1), "none"),
  qt_rule("m/s2", quote("m s"^-2), "none"),
  qt_rule("m/s|m\\.s-1|m s-1", quote("m s"^-1), "none"),
  qt_rule("km2", quote("km"^2), "none"),
  qt_rule("g/km", quote("g km"^-1), "none"),
  qt_rule("g/s", quote("g s"^-1), "none"),
  qt_rule("g/hour|g/hr", quote("g hour"^-1), "none"),
  qt_rule("g/kg", quote("g kg"^-1), "none"),
  qt_rule("kW/t", quote("kW t"^-1), "none")
)

# combine all rules into one regular expression, with each rule in its own
# capturing group so the matching rule can be identified
quicktext_regex <- paste0(
  "(",
  vapply(
    quicktext_rules,
    \(rule) {
      switch(
        rule$boundary,
        name = paste0("(?<![A-Za-z])(?:", rule$pattern, ")(?![A-Za-z0-9])"),
        word = paste0("\\b(?:", rule$pattern, ")\\b"),
        none = paste0("(?:", rule$pattern, ")")
      )
    },
    character(1)
  ),
  ")",
  collapse = "|"
)

# Formatting helpers -------------------------------------------------------

# Format a single string, returning an expression (`parse = TRUE`) or plotmath
# code as a string (`parse = FALSE`)
quicktext_format <- function(text, parse) {
  if (!parse) {
    # plotmath code strings are used for facet labels, which are kept to one
    # line
    expr <- quicktext_line(gsub("\n", " ", text, fixed = TRUE))
    return(paste(deparse(expr, width.cutoff = 500L), collapse = ""))
  }

  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  # `strsplit()` drops a trailing empty line
  if (endsWith(text, "\n") || length(lines) == 0L) {
    lines <- c(lines, "")
  }

  exprs <- lapply(lines, quicktext_line)

  if (length(exprs) == 1L) {
    exprs[[1]]
  } else {
    quicktext_stack(exprs)
  }
}

# Format a single line of text as a plotmath `paste()` call, in which unmatched
# text is kept as strings and matched text is replaced by each rule's `expr`
quicktext_line <- function(line) {
  if (!nzchar(line)) {
    return(quote(paste()))
  }

  matches <- gregexpr(quicktext_regex, line, perl = TRUE)[[1]]
  if (matches[1] == -1L) {
    return(call("paste", line))
  }

  starts <- as.integer(matches)
  ends <- starts + attr(matches, "match.length") - 1L
  # the matching rule is the capturing group that matched. Groups that didn't
  # match have zero length (no pattern can match an empty string)
  rule_ids <- apply(attr(matches, "capture.length") > 0L, 1, which.max)

  pieces <- list()
  pos <- 1L
  for (i in seq_along(starts)) {
    if (starts[i] > pos) {
      pieces[[length(pieces) + 1L]] <- substr(line, pos, starts[i] - 1L)
    }
    pieces[[length(pieces) + 1L]] <- quicktext_rules[[rule_ids[i]]]$expr
    pos <- ends[i] + 1L
  }
  if (pos <= nchar(line)) {
    pieces[[length(pieces) + 1L]] <- substr(line, pos, nchar(line))
  }

  as.call(c(as.name("paste"), pieces))
}

# Stack plotmath expressions as lines. Plotmath has no line break, but `atop()`
# stacks two items, so it is nested to stack any number of lines, e.g.,
# `atop(displaystyle(a), displaystyle(atop(displaystyle(b), displaystyle(c))))`.
# Each part is wrapped in `displaystyle()` as nested `atop()` otherwise shrinks
# each level's text
quicktext_stack <- function(exprs) {
  if (length(exprs) == 1L) {
    return(call("displaystyle", exprs[[1]]))
  }
  call(
    "displaystyle",
    call(
      "atop",
      call("displaystyle", exprs[[1]]),
      quicktext_stack(exprs[-1])
    )
  )
}
