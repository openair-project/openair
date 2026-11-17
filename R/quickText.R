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
#' @param ... Additional arguments. `parse = FALSE` returns the formatted text
#'   as a character vector of plotmath code (e.g., for
#'   [ggplot2::label_parsed()]) rather than as an expression.
#' @export
#' @return An expression for graphical evaluation, with one element per element
#'   of `text`. If `parse = FALSE`, a character vector of plotmath code. `NULL`
#'   is returned unchanged.
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

  # `NULL` means "no label"; an empty expression would still be drawn, taking up
  # space and replacing default axis titles
  if (is.null(text)) {
    return(NULL)
  }

  if (is.expression(text)) {
    if (parse) {
      return(text)
    }
    return(purrr::map_chr(as.list(text), quicktext_deparse))
  }

  # coerce anything else (e.g., `NA`, numbers, factors) to character
  text <- paste(text)

  if (parse) {
    as.expression(purrr::map(text, quicktext_format, parse = TRUE))
  } else {
    purrr::map_chr(text, quicktext_format, parse = FALSE)
  }
}

# Formatting rules ---------------------------------------------------------

# Where a spelling may match, given as regular expressions checked `before` and
# `after` it:
#
#   - "name": not preceded by a letter, nor followed by a letter, digit or
#   decimal part. This stops, e.g., "pm100", "pm1.5" or "year2020" being
#   formatted, but allows underscores either side so that names like
#   "no2_roadside" are formatted.
#
#   - "standalone": preceded by the start of the text, whitespace, "(", "[" or
#   "/", and followed by the end of the text, whitespace, ")", "]", ",", ";",
#   ":" or "/". For short names that are also common words or prefixes, so that,
#   e.g., "co-located" and "no. 5" are not formatted.
#
#   - "prefix": not preceded by a letter, nor followed by a lowercase letter, so
#   it can prefix a name (e.g., "DeltaNO2") but not start a word (e.g.,
#   "Deltaville").
#
#   - "word": a regular expression word boundary (`\b`) either side, so
#   underscores also prevent a match (e.g., "ws_max" is not formatted).
#
#   - "unit": anything before, but not followed by a letter or digit. This
#   allows, e.g., the "m/s" in "km/s", but stops "m/sec" being formatted as
#   "m s^-1 ec" or "km25" as "km^2 5".
#
#   - "none": matched anywhere, e.g., the "degrees" in "degreesF".
quicktext_boundaries <- dplyr::tribble(
  ~boundary    , ~before              , ~after                   ,
  "name"       , "(?<![A-Za-z])"      , "(?![A-Za-z0-9]|\\.[0-9])" ,
  "standalone" , "(?<![^\\s(\\[/])"   , "(?![^\\s)\\],;:/])"     ,
  "prefix"     , "(?<![A-Za-z])"      , "(?![a-z])"              ,
  "word"       , "\\b"                , "\\b"                    ,
  "unit"       , ""                   , "(?![A-Za-z0-9])"        ,
  "none"       , ""                   , ""
)

# Each rule replaces any of its `spellings` (plain text, not regular
# expressions) with a plotmath `expr`, wherever its `boundary` allows (see
# above). It returns one row per spelling.
#
# Where spellings could match at the same position, the longest wins (e.g.,
# "m/s2" over "m/s"), so the order of the rules doesn't matter. Each spelling
# may only appear in one rule.
qt_rule <- function(spellings, expr, boundary = "name") {
  dplyr::tibble(spelling = spellings, expr = list(expr), boundary = boundary)
}

# Spellings using "u" for micro, plus the same spellings written with the micro
# sign or Greek mu (escaped to keep the package source ASCII)
qt_micro <- function(spellings) {
  stem <- sub("^u", "", spellings)
  c(spellings, paste0("\u00b5", stem), paste0("\u03bc", stem))
}

quicktext_rules <- dplyr::bind_rows(
  # gases
  qt_rule(c("NO2", "no2"), quote("NO"[2])),
  qt_rule(c("NO3", "no3"), quote("NO"[3])),
  qt_rule(c("NOX", "NOx", "nox"), quote("NO"[x]), "word"),
  qt_rule("no", "NO", "standalone"),
  qt_rule(c("NH3", "nh3"), quote("NH"[3])),
  qt_rule(c("CO2", "co2"), quote("CO"[2])),
  qt_rule("co", "CO", "standalone"),
  qt_rule("nmhc", "NMHC"),
  qt_rule(c("O3", "o3", "ozone"), quote("O"[3])),
  qt_rule(c("SO2", "so2"), quote("SO"[2])),
  qt_rule(c("H2S", "h2s"), quote("H"[2] * "S")),
  qt_rule(c("CH4", "ch4"), quote("CH"[4])),

  # particulate matter
  qt_rule(c("PM10", "pm10"), quote("PM"[10])),
  qt_rule(c("PM1", "pm1"), quote("PM"[1])),
  qt_rule(c("PM4", "pm4"), quote("PM"[4])),
  qt_rule(c("PM2.5", "pm2.5", "PM25", "pm25"), quote("PM"[2.5])),
  qt_rule(c("PMtot", "pmtot"), quote("PM"[total])),
  qt_rule(c("PMcoarse", "pmcoarse", "PMc", "pmc"), quote("PM"[coarse])),
  qt_rule(c("PMfine", "pmfine", "PMf", "pmf"), quote("PM"[fine])),

  # meteorology
  qt_rule(c("WS", "ws"), "wind spd.", "word"),
  qt_rule(c("WD", "wd"), "wind dir.", "word"),
  qt_rule("air_temp", "temperature", "word"),
  qt_rule("rh", "relative humidity", "word"),

  # statistics and symbols
  qt_rule(c("R2", "r2"), quote("R"^2)),
  qt_rule("tau", quote(tau)),
  qt_rule(c("Delta", "delta"), quote(Delta), "prefix"),
  qt_rule(
    c("degreesC", "degreeC", "dgrC", "deg. C"),
    quote(degree * "C"),
    "unit"
  ),
  qt_rule("degrees", quote(degree), "none"),

  # concentrations
  qt_rule(
    qt_micro(c("ug/m3", "ug.m-3", "ug m-3", "ugm-3")),
    quote(mu * "g m"^-3),
    "unit"
  ),
  qt_rule(c("mg/m3", "mg.m-3", "mg m-3", "mgm-3"), quote("mg m"^-3), "unit"),
  qt_rule(c("ng/m3", "ng.m-3", "ng m-3", "ngm-3"), quote("ng m"^-3), "unit"),
  qt_rule("g/m3", quote("g m"^-3), "unit"),
  qt_rule("cm-3", quote("cm"^-3), "unit"),
  qt_rule(qt_micro("umol/m2/s"), quote(mu * "mol m"^-2 * " s"^-1), "unit"),
  qt_rule(qt_micro("umol/m2"), quote(mu * "mol m"^-2), "unit"),

  # speeds and rates
  qt_rule(
    c("km/hour/s", "km/hr/s", "km/h/s"),
    quote("km hr"^-1 * " s"^-1),
    "unit"
  ),
  qt_rule(c("km/hour", "km/hr", "km/h"), quote("km hr"^-1), "unit"),
  qt_rule("m/s2", quote("m s"^-2), "unit"),
  qt_rule(c("m/s", "m.s-1", "m s-1"), quote("m s"^-1), "unit"),
  qt_rule("km2", quote("km"^2), "unit"),
  qt_rule("g/km", quote("g km"^-1), "unit"),
  qt_rule("g/s", quote("g s"^-1), "unit"),
  qt_rule(c("g/hour", "g/hr"), quote("g hour"^-1), "unit"),
  qt_rule("g/kg", quote("g kg"^-1), "unit"),
  qt_rule("kW/t", quote("kW t"^-1), "unit")
)

# Look up the formatted `expr` for a matched spelling, e.g.,
# `quicktext_lookup[["ug/m3"]]`
quicktext_lookup <- local({
  clashes <- quicktext_rules |>
    dplyr::count(.data$spelling) |>
    dplyr::filter(.data$n > 1L)

  if (nrow(clashes) > 0L) {
    cli::cli_abort(
      "{.fn quickText} spellings in more than one rule: {.val {clashes$spelling}}"
    )
  }

  rlang::set_names(quicktext_rules$expr, quicktext_rules$spelling)
})

# One regular expression that matches any spelling, so each piece of text is
# matched once and replacements are never re-matched by other rules
quicktext_regex <- quicktext_rules |>
  # errors if a rule uses an unknown boundary
  dplyr::inner_join(
    quicktext_boundaries,
    by = "boundary",
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) |>
  # the first alternative that matches wins, so try the longest first
  dplyr::arrange(dplyr::desc(nchar(.data$spelling))) |>
  # `\Q...\E` matches the text between them literally (e.g., "." is a dot)
  dplyr::mutate(
    pattern = paste0(.data$before, "\\Q", .data$spelling, "\\E", .data$after)
  ) |>
  dplyr::pull(.data$pattern) |>
  paste(collapse = "|")

# Formatting helpers -------------------------------------------------------

# Format a single string, returning an expression (`parse = TRUE`) or plotmath
# code as a string (`parse = FALSE`)
quicktext_format <- function(text, parse) {
  if (!parse) {
    # plotmath code strings are used for facet labels, which are kept to one
    # line
    expr <- quicktext_line(gsub("\r?\n", " ", text))
    return(quicktext_deparse(expr))
  }

  # "\r\n" for text with Windows line endings
  lines <- strsplit(text, "\r?\n")[[1]]
  # `strsplit()` drops a trailing empty line
  if (endsWith(text, "\n") || length(lines) == 0L) {
    lines <- c(lines, "")
  }

  exprs <- purrr::map(lines, quicktext_line)

  if (length(exprs) == 1L) {
    exprs[[1]]
  } else {
    quicktext_stack(exprs)
  }
}

# Turn a plotmath expression into one string of plotmath code
quicktext_deparse <- function(expr) {
  paste(deparse(expr, width.cutoff = 500L), collapse = "")
}

# Format a single line of text as a plotmath `paste()` call, in which unmatched
# text is kept as strings and matched text is replaced by its rule's `expr`,
# e.g., "NO2 (ug/m3)" -> `paste(NO[2], " (", mu * "g m"^-3, ")")`
quicktext_line <- function(line) {
  matches <- gregexpr(quicktext_regex, line, perl = TRUE)

  # the matched text, and the text around it (always one more piece)
  matched <- regmatches(line, matches)[[1]]
  between <- regmatches(line, matches, invert = TRUE)[[1]]

  # interleave them, swapping each match for its formatted expression:
  # between[1], match 1, between[2], match 2, between[3], ...
  # (base R rather than purrr here, as this runs for every line of every label
  # and `purrr::map2()` + `purrr::list_flatten()` is ~50x slower)
  pieces <- list(between[1])
  for (i in seq_along(matched)) {
    pieces <- c(pieces, list(quicktext_lookup[[matched[i]]], between[i + 1]))
  }

  # drop empty strings, e.g., when a match is at the start or end of the line
  pieces <- Filter(\(piece) !identical(piece, ""), pieces)

  bquote(paste(..(pieces)), splice = TRUE)
}

# Stack plotmath expressions as lines. Plotmath has no line break, but `atop()`
# stacks two items, so it is nested to stack any number of lines, working back
# from the last line, e.g.,
# `atop(displaystyle(a), displaystyle(atop(displaystyle(b), displaystyle(c))))`.
# Each part is wrapped in `displaystyle()` as nested `atop()` otherwise shrinks
# each level's text
quicktext_stack <- function(exprs) {
  exprs <- purrr::map(exprs, \(expr) call("displaystyle", expr))
  purrr::reduce(
    exprs,
    \(line, below) call("displaystyle", call("atop", line, below)),
    .dir = "backward"
  )
}

# ggplot2 helpers ----------------------------------------------------------

# `quickText()` for ggplot2 labels (e.g., `scale_*(labels = )`). Empty input is
# returned unchanged (scales may have no breaks), and results are named after
# `x` (or keep its names), as scales can match labels by name
label_openair <- function(x, parse = TRUE, auto_text = TRUE) {
  if (!auto_text || length(x) == 0L) {
    return(x)
  }

  out <- quickText(x, parse = parse)
  names(out) <- names(x) %||%
    if (is.character(x) || is.factor(x)) as.character(x)
  out
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
